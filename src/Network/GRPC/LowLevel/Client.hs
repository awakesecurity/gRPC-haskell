{-# LANGUAGE RecordWildCards #-}

-- | This module defines data structures and operations pertaining to registered
-- clients using registered calls; for unregistered support, see
-- `Network.GRPC.LowLevel.Client.Unregistered`.
module Network.GRPC.LowLevel.Client where

import           Control.Exception                     (bracket, finally)
import           Control.Monad                         (join)
import           Data.ByteString                       (ByteString)
import           Foreign.Ptr                           (nullPtr)
import qualified Network.GRPC.Unsafe                   as C
import qualified Network.GRPC.Unsafe.Constants         as C
import qualified Network.GRPC.Unsafe.Op                as C
import qualified Network.GRPC.Unsafe.Time              as C

import           Network.GRPC.LowLevel.Call
import           Network.GRPC.LowLevel.CompletionQueue
import           Network.GRPC.LowLevel.GRPC
import           Network.GRPC.LowLevel.Op

-- | Represents the context needed to perform client-side gRPC operations.
data Client = Client {clientChannel :: C.Channel,
                      clientCQ      :: CompletionQueue,
                      clientConfig  :: ClientConfig
                     }

-- | Configuration necessary to set up a client.
data ClientConfig = ClientConfig {serverHost :: Host,
                                  serverPort :: Port}

clientEndpoint :: ClientConfig -> Endpoint
clientEndpoint ClientConfig{..} = endpoint serverHost serverPort

createClient :: GRPC -> ClientConfig -> IO Client
createClient grpc clientConfig = do
  let Endpoint e = clientEndpoint clientConfig
  clientChannel <- C.grpcInsecureChannelCreate e nullPtr C.reserved
  clientCQ <- createCompletionQueue grpc
  return Client{..}

destroyClient :: Client -> IO ()
destroyClient Client{..} = do
  shutdownResult <- shutdownCompletionQueue clientCQ
  case shutdownResult of
    Left x -> do putStrLn $ "Failed to stop client CQ: " ++ show x
                 putStrLn $ "Trying to shut down anyway."
    Right _ -> return ()
  C.grpcChannelDestroy clientChannel

withClient :: GRPC -> ClientConfig -> (Client -> IO a) -> IO a
withClient grpc config = bracket (createClient grpc config)
                                 (\c -> grpcDebug "withClient: destroying."
                                        >> destroyClient c)

clientConnectivity :: Client -> IO C.ConnectivityState
clientConnectivity Client{..} =
  C.grpcChannelCheckConnectivityState clientChannel False

-- | Register a method on the client so that we can call it with
-- 'clientRequest'.
clientRegisterMethod :: Client
                        -> MethodName
                        -- ^ method name, e.g. "/foo"
                        -> GRPCMethodType
                        -> IO RegisteredMethod
clientRegisterMethod Client{..} meth Normal = do
  let e = clientEndpoint clientConfig
  handle <- C.grpcChannelRegisterCall clientChannel
              (unMethodName meth) (unEndpoint e) C.reserved
  return $ RegisteredMethod Normal meth e handle
clientRegisterMethod _ _ _ = error "Streaming methods not yet implemented."

-- | Create a new call on the client for a registered method.
-- Returns 'Left' if the CQ is shutting down or if the job to create a call
-- timed out.
clientCreateCall :: Client
                 -> RegisteredMethod
                 -> TimeoutSeconds
                 -> IO (Either GRPCIOError ClientCall)
clientCreateCall Client{..} RegisteredMethod{..} timeout = do
  let parentCall = C.Call nullPtr --Unsure what this does. null is safe, though.
  C.withDeadlineSeconds timeout $ \deadline -> do
    channelCreateCall clientChannel parentCall C.propagateDefaults
      clientCQ methodHandle deadline

-- TODO: the error-handling refactor made this quite ugly. It could be fixed
-- by switching to ExceptT IO.
-- | Handles safe creation and cleanup of a client call
withClientCall :: Client
               -> RegisteredMethod
               -> TimeoutSeconds
               -> (ClientCall -> IO (Either GRPCIOError a))
               -> IO (Either GRPCIOError a)
withClientCall client regmethod timeout f = do
  createResult <- clientCreateCall client regmethod timeout
  case createResult of
    Left x -> return $ Left x
    Right call -> f call `finally` logDestroy call
      where logDestroy c = grpcDebug "withClientCall(R): destroying."
                           >> destroyClientCall c

data NormalRequestResult = NormalRequestResult
  { rspBody :: ByteString
  , initMD  :: Maybe MetadataMap -- initial metadata
  , trailMD :: MetadataMap       -- trailing metadata
  , rspCode :: C.StatusCode
  , details :: StatusDetails
  }
  deriving (Show, Eq)

-- | Function for assembling call result when the 'MethodType' is 'Normal'.
compileNormalRequestResults :: [OpRecvResult]
                               -> Either GRPCIOError NormalRequestResult
compileNormalRequestResults
  [OpRecvInitialMetadataResult m,
   OpRecvMessageResult (Just body),
   OpRecvStatusOnClientResult m2 status details]
    = Right $ NormalRequestResult body (Just m) m2 status
                                  (StatusDetails details)
  -- TODO: it seems registered request responses on the server
  -- don't send initial metadata. Hence the 'Maybe'. Investigate.
compileNormalRequestResults
  [OpRecvMessageResult (Just body),
   OpRecvStatusOnClientResult m2 status details]
    = Right $ NormalRequestResult body Nothing m2 status (StatusDetails details)
compileNormalRequestResults x =
  case extractStatusInfo x of
    Nothing -> Left GRPCIOUnknownError
    Just (_meta, status, details) ->
      Left (GRPCIOBadStatusCode status (StatusDetails details))

-- | Make a request of the given method with the given body. Returns the
-- server's response. TODO: This is preliminary until we figure out how many
-- different variations on sending request ops will be needed for full gRPC
-- functionality.
clientRequest :: Client
              -> RegisteredMethod
              -> TimeoutSeconds
              -- ^ Timeout of both the grpc_call and the max time to wait for
              -- the completion of the batch. TODO: I think we will need to
              -- decouple the lifetime of the call from the queue deadline once
              -- we expose functionality for streaming calls, where one call
              -- object persists across many batches.
              -> ByteString
              -- ^ The body of the request
              -> MetadataMap
              -- ^ Metadata to send with the request
              -> IO (Either GRPCIOError NormalRequestResult)
clientRequest client@(Client{..}) rm@(RegisteredMethod{..})
              timeLimit body meta =
  fmap join $ case methodType of
    Normal -> withClientCall client rm timeLimit $ \call -> do
                grpcDebug "clientRequest(R): created call."
                debugClientCall call
                let call' = unClientCall call
                -- NOTE: sendOps and recvOps *must* be in separate batches or
                -- the client hangs when the server can't be reached.
                let sendOps = [OpSendInitialMetadata meta
                           , OpSendMessage body
                           , OpSendCloseFromClient]
                sendRes <- runOps call' clientCQ sendOps timeLimit
                case sendRes of
                  Left x -> do grpcDebug "clientRequest(R) : batch error."
                               return $ Left x
                  Right rs -> do
                    let recvOps = [OpRecvInitialMetadata,
                                   OpRecvMessage,
                                   OpRecvStatusOnClient]
                    recvRes <- runOps call' clientCQ recvOps timeLimit
                    case recvRes of
                      Left x -> do
                        grpcDebug "clientRequest(R): batch error."
                        return $ Left x
                      Right rs' -> do
                        return $ Right $ compileNormalRequestResults (rs ++ rs')
    _ -> error "Streaming methods not yet implemented."

clientNormalRequestOps :: ByteString -> MetadataMap -> [Op]
clientNormalRequestOps body metadata =
  [OpSendInitialMetadata metadata,
   OpSendMessage body,
   OpSendCloseFromClient,
   OpRecvInitialMetadata,
   OpRecvMessage,
   OpRecvStatusOnClient]
