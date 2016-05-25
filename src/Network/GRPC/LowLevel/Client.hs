{-# LANGUAGE RecordWildCards #-}

module Network.GRPC.LowLevel.Client where

import           Control.Exception (bracket, finally)
import           Data.ByteString (ByteString)
import           Foreign.Ptr (nullPtr)
import qualified Network.GRPC.Unsafe as C
import qualified Network.GRPC.Unsafe.Time as C
import qualified Network.GRPC.Unsafe.Constants as C
import qualified Network.GRPC.Unsafe.Op as C

import           Network.GRPC.LowLevel.GRPC
import           Network.GRPC.LowLevel.CompletionQueue
import           Network.GRPC.LowLevel.Call
import           Network.GRPC.LowLevel.Op

-- | Represents the context needed to perform client-side gRPC operations.
data Client = Client {clientChannel :: C.Channel,
                      clientCQ :: CompletionQueue}

-- | Configuration necessary to set up a client.
data ClientConfig = ClientConfig {clientServerHost :: Host,
                                  clientServerPort :: Int}

createClient :: GRPC -> ClientConfig -> IO Client
createClient grpc ClientConfig{..} = do
  let hostPort = (unHost clientServerHost) ++ ":" ++ (show clientServerPort)
  chan <- C.grpcInsecureChannelCreate hostPort nullPtr C.reserved
  cq <- createCompletionQueue grpc
  return $ Client chan cq

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

-- | Register a method on the client so that we can call it with
-- 'clientRegisteredRequest'.
clientRegisterMethod :: Client
                        -> MethodName
                        -- ^ method name, e.g. "/foo"
                        -> Host
                        -- ^ host name, e.g. "localhost"
                        -> GRPCMethodType
                        -> IO RegisteredMethod
clientRegisterMethod Client{..} name host Normal = do
  handle <- C.grpcChannelRegisterCall clientChannel (unMethodName name)
                                      (unHost host) C.reserved
  return $ RegisteredMethod Normal name host handle
clientRegisterMethod _ _ _ _ = error "Streaming methods not yet implemented."

-- | Create a new call on the client for a registered method.
-- Returns 'Left' if the CQ is shutting down or if the job to create a call
-- timed out.
clientCreateRegisteredCall :: Client -> RegisteredMethod -> TimeoutSeconds
                    -> IO (Either GRPCIOError Call)
clientCreateRegisteredCall Client{..} RegisteredMethod{..} timeout = do
  let parentCall = C.Call nullPtr --Unsure what this does. null is safe, though.
  C.withDeadlineSeconds timeout $ \deadline -> do
    channelCreateRegisteredCall clientChannel parentCall C.propagateDefaults
                                clientCQ methodHandle deadline

-- TODO: the error-handling refactor made this quite ugly. It could be fixed
-- by switching to ExceptT IO.
-- | Handles safe creation and cleanup of a client call
withClientRegisteredCall :: Client -> RegisteredMethod -> TimeoutSeconds
                            -> (Call
                                -> IO (Either GRPCIOError a))
                            -> IO (Either GRPCIOError a)
withClientRegisteredCall client regmethod timeout f = do
  createResult <- clientCreateRegisteredCall client regmethod timeout
  case createResult of
    Left x -> return $ Left x
    Right call -> f call `finally` logDestroy call
      where logDestroy c = grpcDebug "withClientRegisteredCall: destroying."
                           >> destroyCall c

-- | Create a call on the client for an endpoint without using the
-- method registration machinery. In practice, we'll probably only use the
-- registered method version, but we include this for completeness and testing.
clientCreateCall :: Client
                    -> MethodName
                    -- ^ The method name
                    -> Host
                    -- ^ The host.
                    -> TimeoutSeconds
                    -> IO (Either GRPCIOError Call)
clientCreateCall Client{..} method host timeout = do
  let parentCall = C.Call nullPtr
  C.withDeadlineSeconds timeout $ \deadline -> do
    channelCreateCall clientChannel parentCall C.propagateDefaults
                      clientCQ method host deadline

withClientCall :: Client -> MethodName -> Host -> TimeoutSeconds
                  -> (Call -> IO (Either GRPCIOError a))
                  -> IO (Either GRPCIOError a)
withClientCall client method host timeout f = do
  createResult <- clientCreateCall client method host timeout
  case createResult of
    Left x -> return $ Left x
    Right call -> f call `finally` logDestroy call
                    where logDestroy c = grpcDebug "withClientCall: destroying."
                                         >> destroyCall c

data NormalRequestResult = NormalRequestResult
                             ByteString
                             (Maybe MetadataMap) --init metadata
                             MetadataMap --trailing metadata
                             C.StatusCode
                             StatusDetails
  deriving (Show, Eq)

-- | Function for assembling call result when the 'MethodType' is 'Normal'.
compileNormalRequestResults :: [OpRecvResult] -> NormalRequestResult
compileNormalRequestResults
  --TODO: consider using more precise type instead of match.
  -- Whether we do so depends on whether this layer of abstraction is supposed
  -- to be a safe interface to the gRPC C core library, or something that makes
  -- core use cases easy.
  [OpRecvInitialMetadataResult m,
   OpRecvMessageResult body,
   OpRecvStatusOnClientResult m2 status details]
    = NormalRequestResult body (Just m) m2 status (StatusDetails details)
  -- TODO: it seems registered request responses on the server
  -- don't send initial metadata. Hence the 'Maybe'. Investigate.
compileNormalRequestResults
  [OpRecvMessageResult body,
   OpRecvStatusOnClientResult m2 status details]
    = NormalRequestResult body Nothing m2 status (StatusDetails details)
compileNormalRequestResults _ =
  --TODO: impossible case should be enforced by more precise types.
  error "non-normal request input to compileNormalRequestResults."


-- | Make a request of the given method with the given body. Returns the
-- server's response. TODO: This is preliminary until we figure out how many
-- different variations on sending request ops will be needed for full gRPC
-- functionality.
clientRegisteredRequest :: Client
                           -> RegisteredMethod
                           -> TimeoutSeconds
                           -- ^ Timeout of both the grpc_call and the
                           -- max time to wait for the completion of the batch.
                           -- TODO: I think we will need to decouple the
                           -- lifetime of the call from the queue deadline once
                           -- we expose functionality for streaming calls, where
                           -- one call object persists across many batches.
                           -> ByteString
                           -- ^ The body of the request.
                           -> MetadataMap
                           -- ^ Metadata to send with the request.
                           -> IO (Either GRPCIOError NormalRequestResult)
clientRegisteredRequest client@(Client{..}) rm@(RegisteredMethod{..})
                        timeLimit body meta =
  case methodType of
    Normal -> withClientRegisteredCall client rm timeLimit $ \call -> do
                grpcDebug "clientRegisteredRequest: created call."
                debugCall call
                --TODO: doing one op at a time to debug. Some were hanging.
                let op1 = [OpSendInitialMetadata meta]
                res1 <- runOps call clientCQ op1 timeLimit
                grpcDebug $ "finished res1: " ++ show res1
                let op2 = [OpSendMessage body]
                res2 <- runOps call clientCQ op2 timeLimit
                grpcDebug $ "finished res2: " ++ show res2
                let op3 = [OpSendCloseFromClient]
                res3 <- runOps call clientCQ op3 timeLimit
                grpcDebug $ "finished res3: " ++ show res3
                let op4 = [OpRecvMessage]
                res4 <- runOps call clientCQ op4 timeLimit
                grpcDebug $ "finished res4: " ++ show res4
                let op5 = [OpRecvStatusOnClient]
                res5 <- runOps call clientCQ op5 timeLimit
                grpcDebug $ "finished res5: " ++ show res5
                let results = do
                              r1 <- res1
                              r2 <- res2
                              r3 <- res3
                              r4 <- res4
                              r5 <- res5
                              return $ r1 ++ r2 ++ r3 ++ r4 ++ r5
                case results of
                  Left x -> return $ Left x
                  Right rs -> return $
                                Right $ compileNormalRequestResults rs
    _ -> error "Streaming methods not yet implemented."

-- | Makes a normal (non-streaming) request without needing to register a method
-- first. Probably only useful for testing. TODO: This is preliminary, like
-- 'clientRegisteredRequest'.
clientRequest :: Client
                 -> MethodName
                 -- ^ Method name, e.g. "/foo"
                 -> Host
                 -- ^ Host. Not sure if used.
                 -> TimeoutSeconds
                 -> ByteString
                 -- ^ Request body.
                 -> MetadataMap
                 -- ^ Request metadata.
                 -> IO (Either GRPCIOError NormalRequestResult)
clientRequest client@(Client{..}) (MethodName method) (Host host)
              timeLimit body meta = do
  withClientCall client (MethodName method) (Host host) timeLimit $ \call -> do
    let ops = clientNormalRequestOps body meta
    results <- runOps call clientCQ ops timeLimit
    grpcDebug "clientRequest: ops ran."
    case results of
      Left x -> return $ Left x
      Right rs -> return $ Right $ compileNormalRequestResults rs


clientNormalRequestOps :: ByteString -> MetadataMap -> [Op]
clientNormalRequestOps body metadata =
  [OpSendInitialMetadata metadata,
   OpSendMessage body,
   OpSendCloseFromClient,
   OpRecvInitialMetadata,
   OpRecvMessage,
   OpRecvStatusOnClient]
