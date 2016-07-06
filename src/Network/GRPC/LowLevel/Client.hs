{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms   #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE ViewPatterns      #-}

-- | This module defines data structures and operations pertaining to registered
-- clients using registered calls; for unregistered support, see
-- `Network.GRPC.LowLevel.Client.Unregistered`.
module Network.GRPC.LowLevel.Client where

import           Control.Arrow
import           Control.Exception                     (bracket, finally)
import           Control.Monad
import           Control.Monad.Trans.Class             (MonadTrans(lift))
import           Control.Monad.Trans.Except
import           Data.ByteString                       (ByteString)
import           Network.GRPC.LowLevel.Call
import           Network.GRPC.LowLevel.CompletionQueue
import           Network.GRPC.LowLevel.GRPC
import           Network.GRPC.LowLevel.Op
import qualified Network.GRPC.Unsafe                   as C
import qualified Network.GRPC.Unsafe.ChannelArgs       as C
import qualified Network.GRPC.Unsafe.Constants         as C
import qualified Network.GRPC.Unsafe.Op                as C
import qualified Network.GRPC.Unsafe.Time              as C

import qualified Pipes                                 as P
import qualified Pipes.Core                            as P

-- | Represents the context needed to perform client-side gRPC operations.
data Client = Client {clientChannel :: C.Channel,
                      clientCQ      :: CompletionQueue,
                      clientConfig  :: ClientConfig
                     }

-- | Configuration necessary to set up a client.
data ClientConfig = ClientConfig {serverHost :: Host,
                                  serverPort :: Port,
                                  clientArgs :: [C.Arg]
                                  -- ^ Optional arguments for setting up the
                                  -- channel on the client. Supplying an empty
                                  -- list will cause the channel to use gRPC's
                                  -- default options.
                                 }

clientEndpoint :: ClientConfig -> Endpoint
clientEndpoint ClientConfig{..} = endpoint serverHost serverPort

createClient :: GRPC -> ClientConfig -> IO Client
createClient grpc clientConfig =
  C.withChannelArgs (clientArgs clientConfig) $ \chanargs -> do
    let Endpoint e = clientEndpoint clientConfig
    clientChannel <- C.grpcInsecureChannelCreate e chanargs C.reserved
    clientCQ <- createCompletionQueue grpc
    return Client{..}

destroyClient :: Client -> IO ()
destroyClient Client{..} = do
  grpcDebug "destroyClient: calling grpc_channel_destroy()"
  C.grpcChannelDestroy clientChannel
  grpcDebug "destroyClient: shutting down CQ."
  shutdownResult <- shutdownCompletionQueue clientCQ
  case shutdownResult of
    Left x -> do putStrLn $ "Failed to stop client CQ: " ++ show x
                 putStrLn $ "Trying to shut down anyway."
    Right _ -> return ()

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
                     -> GRPCMethodType
                     -> IO (RegisteredMethod mt)
clientRegisterMethod Client{..} meth mty = do
  let e = clientEndpoint clientConfig
  RegisteredMethod mty meth e <$>
    C.grpcChannelRegisterCall clientChannel
      (unMethodName meth) (unEndpoint e) C.reserved

-- | Create a new call on the client for a registered method.
-- Returns 'Left' if the CQ is shutting down or if the job to create a call
-- timed out.
clientCreateCall :: Client
                 -> RegisteredMethod mt
                 -> TimeoutSeconds
                 -> IO (Either GRPCIOError ClientCall)
clientCreateCall c rm ts = clientCreateCallParent c rm ts Nothing

-- | For servers that act as clients to other gRPC servers, this version creates
-- a client call with an optional parent server call. This allows for cascading
-- call cancellation from the `ServerCall` to the `ClientCall`.
clientCreateCallParent :: Client
                           -> RegisteredMethod mt
                           -> TimeoutSeconds
                           -> Maybe ServerCall
                           -- ^ Optional parent call for cascading cancellation.
                           -> IO (Either GRPCIOError ClientCall)
clientCreateCallParent Client{..} RegisteredMethod{..} timeout parent = do
  C.withDeadlineSeconds timeout $ \deadline -> do
    channelCreateCall clientChannel parent C.propagateDefaults
      clientCQ methodHandle deadline

-- | Handles safe creation and cleanup of a client call
withClientCall :: Client
               -> RegisteredMethod mt
               -> TimeoutSeconds
               -> (ClientCall -> IO (Either GRPCIOError a))
               -> IO (Either GRPCIOError a)
withClientCall client regmethod timeout f =
  withClientCallParent client regmethod timeout Nothing f

-- | Handles safe creation and cleanup of a client call, with an optional parent
-- call parameter. This allows for cancellation to cascade from the parent
-- `ServerCall` to the created `ClientCall`. Obviously, this is only useful if
-- the given gRPC client is also a server.
withClientCallParent :: Client
                        -> RegisteredMethod mt
                        -> TimeoutSeconds
                        -> (Maybe ServerCall)
                        -- ^ Optional parent call for cascading cancellation.
                        -> (ClientCall -> IO (Either GRPCIOError a))
                        -> IO (Either GRPCIOError a)
withClientCallParent client regmethod timeout parent f = do
  createResult <- clientCreateCallParent client regmethod timeout parent
  case createResult of
    Left x -> return $ Left x
    Right call -> f call `finally` logDestroy call
      where logDestroy c = grpcDebug "withClientCall(R): destroying."
                           >> destroyClientCall c

data NormalRequestResult = NormalRequestResult
  { rspBody :: ByteString
  , initMD  :: MetadataMap -- ^ initial metadata
  , trailMD :: MetadataMap -- ^ trailing metadata
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
    = Right $ NormalRequestResult body m m2 status (StatusDetails details)
compileNormalRequestResults x =
  case extractStatusInfo x of
    Nothing -> Left GRPCIOUnknownError
    Just (_meta, status, details) ->
      Left (GRPCIOBadStatusCode status (StatusDetails details))

--------------------------------------------------------------------------------
-- clientReader (client side of server streaming mode)

-- | First parameter is initial server metadata.
type ClientReaderHandler = MetadataMap -> StreamRecv -> Streaming ()

clientReader :: Client
             -> RegisteredMethod 'ServerStreaming
             -> TimeoutSeconds
             -> ByteString -- ^ The body of the request
             -> MetadataMap -- ^ Metadata to send with the request
             -> ClientReaderHandler
             -> IO (Either GRPCIOError (MetadataMap, C.StatusCode, StatusDetails))
clientReader cl@Client{ clientCQ = cq } rm tm body initMeta f =
  withClientCall cl rm tm go
  where
    go cc@(unClientCall -> c) = runExceptT $ do
      lift (debugClientCall cc)
      runOps' c cq [ OpSendInitialMetadata initMeta
                   , OpSendMessage body
                   , OpSendCloseFromClient
                   ]
      srvMD <- recvInitialMetadata c cq
      runStreamingProxy "clientReader'" c cq (f srvMD streamRecv)
      recvStatusOnClient c cq

--------------------------------------------------------------------------------
-- clientWriter (client side of client streaming mode)

type ClientWriterHandler = StreamSend -> Streaming ()
type ClientWriterResult  = (Maybe ByteString, MetadataMap, MetadataMap,
                              C.StatusCode, StatusDetails)

clientWriter :: Client
             -> RegisteredMethod 'ClientStreaming
             -> TimeoutSeconds
             -> MetadataMap -- ^ Initial client metadata
             -> ClientWriterHandler
             -> IO (Either GRPCIOError ClientWriterResult)
clientWriter cl rm tm initMeta =
  withClientCall cl rm tm . clientWriterCmn cl initMeta

clientWriterCmn :: Client -- ^ The active client
                -> MetadataMap -- ^ Initial client metadata
                -> ClientWriterHandler
                -> ClientCall -- ^ The active client call
                -> IO (Either GRPCIOError ClientWriterResult)
clientWriterCmn (clientCQ -> cq) initMeta f cc@(unClientCall -> c) =
  runExceptT $ do
    lift (debugClientCall cc)
    sendInitialMetadata c cq initMeta
    runStreamingProxy "clientWriterCmn" c cq (f streamSend)
    sendSingle c cq OpSendCloseFromClient
    let ops = [OpRecvInitialMetadata, OpRecvMessage, OpRecvStatusOnClient]
    runOps' c cq ops >>= \case
      CWRFinal mmsg initMD trailMD st ds
        -> return (mmsg, initMD, trailMD, st, ds)
      _ -> throwE (GRPCIOInternalUnexpectedRecv "clientWriter")

pattern CWRFinal mmsg initMD trailMD st ds
  <- [ OpRecvInitialMetadataResult initMD
     , OpRecvMessageResult mmsg
     , OpRecvStatusOnClientResult trailMD st (StatusDetails -> ds)
     ]

--------------------------------------------------------------------------------
-- clientRW (client side of bidirectional streaming mode)

-- | First parameter is initial server metadata.
type ClientRWHandler = MetadataMap -> StreamRecv -> StreamSend -> Streaming ()

-- | For bidirectional-streaming registered requests
clientRW :: Client
         -> RegisteredMethod 'BiDiStreaming
         -> TimeoutSeconds
         -> MetadataMap
            -- ^ request metadata
         -> ClientRWHandler
         -> IO (Either GRPCIOError (MetadataMap, C.StatusCode, StatusDetails))
clientRW c@Client{ clientCQ = cq } rm tm initMeta f =
  withClientCall c rm tm go
  where
    go cc@(unClientCall -> call) = runExceptT $ do
      lift (debugClientCall cc)
      sendInitialMetadata call cq initMeta
      srvMeta <- recvInitialMetadata call cq
      runStreamingProxy "clientRW" call cq (f srvMeta streamRecv streamSend)
      runOps' call cq [OpSendCloseFromClient] -- WritesDone()
      recvStatusOnClient call cq -- Finish()

--------------------------------------------------------------------------------
-- clientRequest (client side of normal request/response)

-- | Make a request of the given method with the given body. Returns the
-- server's response.
clientRequest :: Client
              -> RegisteredMethod 'Normal
              -> TimeoutSeconds
              -> ByteString
              -- ^ The body of the request
              -> MetadataMap
              -- ^ Metadata to send with the request
              -> IO (Either GRPCIOError NormalRequestResult)
clientRequest c@Client{ clientCQ = cq } rm tm body initMeta =
  withClientCall c rm tm (fmap join . go)
  where
    go cc@(unClientCall -> call) = do
      grpcDebug "clientRequest(R): created call."
      debugClientCall cc
      -- NB: the send and receive operations below *must* be in separate
      -- batches, or the client hangs when the server can't be reached.
      runOps call cq
        [ OpSendInitialMetadata initMeta
        , OpSendMessage body
        , OpSendCloseFromClient
        ]
        >>= \case
          Left x -> do
            grpcDebug "clientRequest(R) : batch error sending."
            return $ Left x
          Right rs ->
            runOps call cq
              [ OpRecvInitialMetadata
              , OpRecvMessage
              , OpRecvStatusOnClient
              ]
              >>= \case
                Left x -> do
                  grpcDebug "clientRequest(R): batch error receiving.."
                  return $ Left x
                Right rs' -> do
                  grpcDebug $ "clientRequest(R): got " ++ show rs'
                  return $ Right $ compileNormalRequestResults (rs ++ rs')
