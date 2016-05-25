{-# LANGUAGE RecordWildCards #-}

module Network.GRPC.LowLevel.Server where

import           Control.Concurrent (threadDelay)
import           Control.Exception (bracket, finally)
import           Control.Monad
import           Data.ByteString (ByteString)
import qualified Data.Map as M
import           Foreign.Ptr (nullPtr)
import           Foreign.Storable (peek)
import qualified Network.GRPC.Unsafe as C
import qualified Network.GRPC.Unsafe.Op as C

import           Network.GRPC.LowLevel.GRPC
import           Network.GRPC.LowLevel.CompletionQueue (CompletionQueue,
                 pluck, serverRegisterCompletionQueue, serverShutdownAndNotify,
                 createCompletionQueue, shutdownCompletionQueue, TimeoutSeconds,
                 serverRequestRegisteredCall, serverRequestCall)
import           Network.GRPC.LowLevel.Call
import           Network.GRPC.LowLevel.Op

import qualified Network.GRPC.Unsafe.ByteBuffer as C
import qualified Network.GRPC.Unsafe.Metadata as C

-- | Wraps various gRPC state needed to run a server.
data Server = Server {internalServer :: C.Server, serverCQ :: CompletionQueue,
                      registeredMethods :: [RegisteredMethod]}

-- | Configuration needed to start a server. There might be more fields that
-- need to be added to this in the future.
data ServerConfig =
  ServerConfig {hostName :: Host,
                -- ^ Name of the host the server is running on. Not sure
                -- how this is used. Setting to "localhost" works fine in tests.
                port :: Int,
                -- ^ Port to listen for requests on.
                methodsToRegister :: [(MethodName, Host, GRPCMethodType)]
                -- ^ List of (method name, method host, method type) tuples
                -- specifying all methods to register. You can also handle
                -- other unregistered methods with `serverHandleNormalCall`.
               }
  deriving (Show, Eq)

startServer :: GRPC -> ServerConfig -> IO Server
startServer grpc ServerConfig{..} = do
  server <- C.grpcServerCreate nullPtr C.reserved
  let hostPort = (unHost hostName) ++ ":" ++ (show port)
  actualPort <- C.grpcServerAddInsecureHttp2Port server hostPort
  when (actualPort /= port) (error $ "Unable to bind port: " ++ (show port))
  cq <- createCompletionQueue grpc
  serverRegisterCompletionQueue server cq
  methods <- forM methodsToRegister $
               \(name, host, mtype) ->
                 serverRegisterMethod server name host mtype
  C.grpcServerStart server
  return $ Server server cq methods

stopServer :: Server -> IO ()
-- TODO: Do method handles need to be freed?
stopServer (Server server cq _) = do
  grpcDebug "stopServer: calling shutdownNotify."
  shutdownNotify
  grpcDebug "stopServer: cancelling all calls."
  C.grpcServerCancelAllCalls server
  grpcDebug "stopServer: call grpc_server_destroy."
  C.grpcServerDestroy server
  grpcDebug "stopServer: shutting down CQ."
  shutdownCQ

  where shutdownCQ = do
          shutdownResult <- shutdownCompletionQueue cq
          case shutdownResult of
            Left _ -> do putStrLn "Warning: completion queue didn't shut down."
                         putStrLn "Trying to stop server anyway."
            Right _ -> return ()
        shutdownNotify = do
          let shutdownTag = C.tag 0
          serverShutdownAndNotify server cq shutdownTag
          shutdownEvent <- pluck cq shutdownTag 30
          case shutdownEvent of
            -- This case occurs when we pluck but the queue is already in the
            -- 'shuttingDown' state, implying we already tried to shut down.
            (Left GRPCIOShutdown) -> error "Called stopServer twice!"
            (Left _) -> error "Failed to stop server."
            (Right _) -> return ()

-- Uses 'bracket' to safely start and stop a server, even if exceptions occur.
withServer :: GRPC -> ServerConfig -> (Server -> IO a) -> IO a
withServer grpc cfg f = bracket (startServer grpc cfg) stopServer f

-- | Register a method on a server. The 'RegisteredMethod' type can then be used
-- to wait for a request to arrive. Note: gRPC claims this must be called before
-- the server is started, so we do it during startup according to the
-- 'ServerConfig'.
serverRegisterMethod :: C.Server
                        -> MethodName
                        -- ^ method name, e.g. "/foo"
                        -> Host
                        -- ^ host name, e.g. "localhost". I have no idea
                        -- why this is needed since we have to supply a host
                        -- name to start a server in the first place. It doesn't
                        -- seem to have any effect, even if it's filled with
                        -- nonsense.
                        -> GRPCMethodType
                        -- ^ Type of method this will be. In the future, this
                        -- will be used to switch to the correct handling logic.
                        -- Currently, the only valid choice is 'Normal'.
                        -> IO RegisteredMethod
serverRegisterMethod internalServer name host Normal = do
  handle <- C.grpcServerRegisterMethod internalServer
                                       (unMethodName name)
                                       (unHost host)
  grpcDebug $ "registered method to handle " ++ show handle
  return $ RegisteredMethod Normal name host handle
serverRegisterMethod _ _ _ _ = error "Streaming methods not implemented yet."

-- | Create a 'Call' with which to wait for the invocation of a registered
-- method.
serverCreateRegisteredCall :: Server -> RegisteredMethod -> TimeoutSeconds
                              -> IO (Either GRPCIOError Call)
serverCreateRegisteredCall Server{..} rm timeLimit =
  serverRequestRegisteredCall internalServer serverCQ timeLimit rm

withServerRegisteredCall :: Server -> RegisteredMethod -> TimeoutSeconds
                            -> (Call
                                -> IO (Either GRPCIOError a))
                            -> IO (Either GRPCIOError a)
withServerRegisteredCall server regmethod timeout f = do
  createResult <- serverCreateRegisteredCall server regmethod timeout
  case createResult of
    Left x -> return $ Left x
    Right call -> f call `finally` logDestroy call
      where logDestroy c = grpcDebug "withServerRegisteredCall: destroying."
                           >> destroyCall c

serverCreateCall :: Server -> TimeoutSeconds
                    -> IO (Either GRPCIOError Call)
serverCreateCall Server{..} timeLimit =
  serverRequestCall internalServer serverCQ timeLimit

withServerCall :: Server -> TimeoutSeconds
                  -> (Call -> IO (Either GRPCIOError a))
                  -> IO (Either GRPCIOError a)
withServerCall server timeout f = do
  createResult <- serverCreateCall server timeout
  case createResult of
    Left x -> return $ Left x
    Right call -> f call `finally` logDestroy call
      where logDestroy c = grpcDebug "withServerCall: destroying."
                           >> destroyCall c

-- | Sequence of 'Op's needed to receive a normal (non-streaming) call.
serverOpsGetNormalCall :: MetadataMap -> [Op]
serverOpsGetNormalCall initMetadata =
  [OpSendInitialMetadata initMetadata,
   OpRecvMessage]

-- | Sequence of 'Op's needed to respond to a normal (non-streaming) call.
serverOpsSendNormalResponse :: ByteString
                               -> MetadataMap
                               -> C.StatusCode
                               -> StatusDetails
                               -> [Op]
serverOpsSendNormalResponse body metadata code details =
  [OpRecvCloseOnServer,
   OpSendMessage body,
   OpSendStatusFromServer metadata code details]

serverOpsSendNormalRegisteredResponse :: ByteString
                                         -> MetadataMap
                                         -- ^ initial metadata
                                         -> MetadataMap
                                         -- ^ trailing metadata
                                         -> C.StatusCode
                                         -> StatusDetails
                                         -> [Op]
serverOpsSendNormalRegisteredResponse
  body initMetadata trailingMeta code details =
  [OpSendInitialMetadata initMetadata,
   OpRecvCloseOnServer,
   OpSendMessage body,
   OpSendStatusFromServer trailingMeta code details]

-- TODO: we will want to replace this with some more general concept that also
-- works with streaming calls in the future.
-- | Wait for and then handle a normal (non-streaming) call.
serverHandleNormalRegisteredCall :: Server
                          -> RegisteredMethod
                          -> TimeoutSeconds
                          -> MetadataMap
                          -- ^ Initial server metadata
                          -> (ByteString -> MetadataMap
                              -> IO (ByteString,
                                     MetadataMap,
                                     MetadataMap,
                                     StatusDetails))
                             -- ^ Handler function takes a request body and
                             -- metadata and returns a response body and
                             -- metadata.
                          -> IO (Either GRPCIOError ())
serverHandleNormalRegisteredCall s@Server{..} rm timeLimit initMetadata f = do
  -- TODO: we use this timeLimit twice, so the max time spent is 2*timeLimit.
  -- Should we just hard-code time limits instead? Not sure if client
  -- programmer cares, since this function will likely just be put in a loop
  -- anyway.
  withServerRegisteredCall s rm timeLimit $ \call -> do
    grpcDebug "serverHandleNormalRegisteredCall: starting batch."
    debugCall call
    case optionalPayload call of
      Nothing -> error "Impossible: not a registered call." --TODO: better types
      Just payloadPtr -> do
        payload <- peek payloadPtr
        requestBody <- C.copyByteBufferToByteString payload
        metadataArray <- peek $ requestMetadataRecv call
        metadata <- C.getAllMetadataArray metadataArray
        (respBody, initMeta, trailingMeta, details) <- f requestBody metadata
        let status = C.GrpcStatusOk
        let respOps = serverOpsSendNormalRegisteredResponse
                        respBody initMeta trailingMeta status details
        respOpsResults <- runOps call serverCQ respOps timeLimit
        grpcDebug "serverHandleNormalRegisteredCall: finished response ops."
        case respOpsResults of
          Left x -> return $ Left x
          Right _ -> return $ Right ()

--  TODO: This is preliminary.
-- We still need to provide the method name to the handler.
-- | Handle one unregistered call.
serverHandleNormalCall :: Server -> TimeoutSeconds
                          -> MetadataMap
                          -- ^ Initial metadata.
                          -> (ByteString -> MetadataMap
                              -> IO (ByteString, MetadataMap, StatusDetails))
                          -- ^ Handler function takes a request body and
                          -- metadata and returns a response body and metadata.
                          -> IO (Either GRPCIOError ())
serverHandleNormalCall s@Server{..} timeLimit initMetadata f = do
  withServerCall s timeLimit $ \call -> do
    grpcDebug "serverHandleNormalCall: starting batch."
    let recvOps = serverOpsGetNormalCall initMetadata
    opResults <- runOps call serverCQ recvOps timeLimit
    case opResults of
      Left x -> return $ Left x
      Right [OpRecvMessageResult body] -> do
        --TODO: we need to get client metadata
        (respBody, respMetadata, details) <- f body M.empty
        let status = C.GrpcStatusOk
        let respOps = serverOpsSendNormalResponse
                        respBody respMetadata status details
        respOpsResults <- runOps call serverCQ respOps timeLimit
        case respOpsResults of
          Left x -> do grpcDebug "serverHandleNormalCall: resp failed."
                       return $ Left x
          Right _ -> grpcDebug "serverHandleNormalCall: ops done."
                     >> return (Right ())
      x -> error $ "impossible pattern match: " ++ show x
