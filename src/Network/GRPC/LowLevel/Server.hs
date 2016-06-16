{-# LANGUAGE RecordWildCards #-}

-- | This module defines data structures and operations pertaining to registered
-- servers using registered calls; for unregistered support, see
-- `Network.GRPC.LowLevel.Server.Unregistered`.
module Network.GRPC.LowLevel.Server where

import           Control.Exception                     (bracket, finally)
import           Control.Monad
import           Data.ByteString                       (ByteString)
import           Foreign.Ptr                           (nullPtr)
import           Network.GRPC.LowLevel.Call
import           Network.GRPC.LowLevel.CompletionQueue (CompletionQueue,
                                                        TimeoutSeconds,
                                                        createCompletionQueue,
                                                        pluck,
                                                        serverRegisterCompletionQueue,
                                                        serverRequestCall,
                                                        serverShutdownAndNotify,
                                                        shutdownCompletionQueue)
import           Network.GRPC.LowLevel.GRPC
import           Network.GRPC.LowLevel.Op
import qualified Network.GRPC.Unsafe                   as C
import qualified Network.GRPC.Unsafe.Op                as C

-- | Wraps various gRPC state needed to run a server.
data Server = Server
  { internalServer    :: C.Server
  , serverCQ          :: CompletionQueue
  , registeredMethods :: [RegisteredMethod]
  , serverConfig      :: ServerConfig
  }

-- | Configuration needed to start a server.
data ServerConfig = ServerConfig
  { host              :: Host
    -- ^ Name of the host the server is running on. Not sure how this is
    -- used. Setting to "localhost" works fine in tests.
  , port              :: Port
    -- ^ Port on which to listen for requests.
  , methodsToRegister :: [(MethodName, GRPCMethodType)]
    -- ^ List of (method name, method type) tuples specifying all methods to
    -- register. You can also handle other unregistered methods with
    -- `serverHandleNormalCall`.
  }
  deriving (Show, Eq)

serverEndpoint :: ServerConfig -> Endpoint
serverEndpoint ServerConfig{..} = endpoint host port

startServer :: GRPC -> ServerConfig -> IO Server
startServer grpc conf@ServerConfig{..} = do
  let e = serverEndpoint conf
  server <- C.grpcServerCreate nullPtr C.reserved
  actualPort <- C.grpcServerAddInsecureHttp2Port server (unEndpoint e)
  when (actualPort /= unPort port) $
    error $ "Unable to bind port: " ++ show port
  cq <- createCompletionQueue grpc
  serverRegisterCompletionQueue server cq
  methods <- forM methodsToRegister $ \(name, mtype) ->
               serverRegisterMethod server name e mtype
  C.grpcServerStart server
  return $ Server server cq methods conf

stopServer :: Server -> IO ()
-- TODO: Do method handles need to be freed?
stopServer (Server server cq _ _) = do
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
          grpcDebug "called serverShutdownAndNotify; plucking."
          shutdownEvent <- pluck cq shutdownTag (Just 30)
          grpcDebug $ "shutdownNotify: got shutdown event" ++ show shutdownEvent
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
                     -> Endpoint
                     -- ^ Endpoint name name, e.g. "localhost:9999". I have no
                     -- idea why this is needed since we have to provide these
                     -- parameters to start a server in the first place. It
                     -- doesn't seem to have any effect, even if it's filled
                     -- with nonsense.
                     -> GRPCMethodType
                     -- ^ Type of method this will be. In the future, this will
                     -- be used to switch to the correct handling logic.
                     -- Currently, the only valid choice is 'Normal'.
                     -> IO RegisteredMethod
serverRegisterMethod internalServer meth e Normal = do
  handle <- C.grpcServerRegisterMethod internalServer
              (unMethodName meth) (unEndpoint e)
  grpcDebug $ "registered method to handle " ++ show handle
  return $ RegisteredMethod Normal meth e handle
serverRegisterMethod _ _ _ _ = error "Streaming methods not implemented yet."

-- | Create a 'Call' with which to wait for the invocation of a registered
-- method.
serverCreateCall :: Server
                 -> RegisteredMethod
                 -> TimeoutSeconds
                 -> IO (Either GRPCIOError ServerCall)
serverCreateCall Server{..} rm timeLimit =
  serverRequestCall internalServer serverCQ timeLimit rm

withServerCall :: Server
               -> RegisteredMethod
               -> TimeoutSeconds
               -> (ServerCall -> IO (Either GRPCIOError a))
               -> IO (Either GRPCIOError a)
withServerCall server regmethod timeout f = do
  createResult <- serverCreateCall server regmethod timeout
  case createResult of
    Left x -> return $ Left x
    Right call -> f call `finally` logDestroy call
      where logDestroy c = grpcDebug "withServerRegisteredCall: destroying."
                           >> destroyServerCall c

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

-- | A handler for an registered server call; bytestring parameter is request
-- body, with the bytestring response body in the result tuple. The first
-- metadata parameter refers to the request metadata, with the two metadata
-- values in the result tuple being the initial and trailing metadata
-- respectively. We pass in the 'ServerCall' so that the server can call
-- 'serverCallCancel' on it if needed.

-- TODO: make a more rigid type for this with a Maybe MetadataMap for the
-- trailing meta, and use it for both kinds of call handlers.
type ServerHandler
  =  ServerCall -> ByteString -> MetadataMap
  -> IO (ByteString, MetadataMap, C.StatusCode, StatusDetails)

-- TODO: we will want to replace this with some more general concept that also
-- works with streaming calls in the future.
-- | Wait for and then handle a normal (non-streaming) call.
serverHandleNormalCall :: Server
                       -> RegisteredMethod
                       -> TimeoutSeconds
                       -> MetadataMap
                       -- ^ Initial server metadata
                       -> ServerHandler
                       -> IO (Either GRPCIOError ())
serverHandleNormalCall s@Server{..} rm timeLimit initMeta f = do
  withServerCall s rm timeLimit $ \call -> do
    grpcDebug "serverHandleNormalCall(R): starting batch."
    debugServerCall call
    payload <- serverCallGetPayload call
    case payload of
      --TODO: what should we do with an empty payload? Have the handler take
      -- @Maybe ByteString@? Need to figure out when/why payload would be empty.
      Nothing -> error "serverHandleNormalCall(R): payload empty."
      Just requestBody -> do
        requestMeta <- serverCallGetMetadata call
        (respBody, trailingMeta, status, details) <- f call
                                                       requestBody
                                                       requestMeta
        let respOps = serverOpsSendNormalRegisteredResponse
                        respBody initMeta trailingMeta status details
        respOpsResults <- runOps (unServerCall call) serverCQ respOps
        grpcDebug "serverHandleNormalCall(R): finished response ops."
        case respOpsResults of
          Left x -> return $ Left x
          Right _ -> return $ Right ()
