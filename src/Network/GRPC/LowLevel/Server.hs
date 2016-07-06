{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms   #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TupleSections     #-}
{-# LANGUAGE ViewPatterns      #-}

-- | This module defines data structures and operations pertaining to registered
-- servers using registered calls; for unregistered support, see
-- `Network.GRPC.LowLevel.Server.Unregistered`.
module Network.GRPC.LowLevel.Server where

import           Control.Arrow
import           Control.Exception                     (bracket, finally)
import           Control.Monad
import           Control.Monad.Trans.Class             (MonadTrans (lift))
import           Control.Monad.Trans.Except
import           Data.ByteString                       (ByteString)
import           Network.GRPC.LowLevel.Call
import           Network.GRPC.LowLevel.CompletionQueue (CompletionQueue,
                                                        createCompletionQueue,
                                                        pluck,
                                                        serverRegisterCompletionQueue,
                                                        serverRequestCall,
                                                        serverShutdownAndNotify,
                                                        shutdownCompletionQueue)
import           Network.GRPC.LowLevel.GRPC
import           Network.GRPC.LowLevel.Op
import qualified Network.GRPC.Unsafe                   as C
import qualified Network.GRPC.Unsafe.ChannelArgs       as C
import qualified Network.GRPC.Unsafe.Op                as C
import qualified Pipes                                 as P
import qualified Pipes.Core                            as P

-- | Wraps various gRPC state needed to run a server.
data Server = Server
  { internalServer       :: C.Server
  , serverCQ             :: CompletionQueue
  , normalMethods        :: [RegisteredMethod 'Normal]
  , sstreamingMethods    :: [RegisteredMethod 'ServerStreaming]
  , cstreamingMethods    :: [RegisteredMethod 'ClientStreaming]
  , bidiStreamingMethods :: [RegisteredMethod 'BiDiStreaming]
  , serverConfig         :: ServerConfig
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
    -- register.
  , serverArgs        :: [C.Arg]
  -- ^ Optional arguments for setting up the channel on the server. Supplying an
  -- empty list will cause the channel to use gRPC's default options.
  }
  deriving (Show, Eq)

serverEndpoint :: ServerConfig -> Endpoint
serverEndpoint ServerConfig{..} = endpoint host port

startServer :: GRPC -> ServerConfig -> IO Server
startServer grpc conf@ServerConfig{..} =
  C.withChannelArgs serverArgs $ \args -> do
    let e = serverEndpoint conf
    server <- C.grpcServerCreate args C.reserved
    actualPort <- C.grpcServerAddInsecureHttp2Port server (unEndpoint e)
    when (actualPort /= unPort port) $
      error $ "Unable to bind port: " ++ show port
    cq <- createCompletionQueue grpc
    serverRegisterCompletionQueue server cq

    -- Register methods according to their GRPCMethodType kind. It's a bit ugly
    -- to partition them this way, but we get very convenient phantom typing
    -- elsewhere by doing so.
    (ns, ss, cs, bs) <- do
      let f (ns, ss, cs, bs) (nm, mt) = do
            let reg = serverRegisterMethod server nm e mt
            case mt of
              Normal          -> (  , ss, cs, bs) . (:ns) <$> reg
              ServerStreaming -> (ns,   , cs, bs) . (:ss) <$> reg
              ClientStreaming -> (ns, ss,   , bs) . (:cs) <$> reg
              BiDiStreaming   -> (ns, ss, cs,   ) . (:bs) <$> reg
      foldM f ([],[],[],[]) methodsToRegister

    C.grpcServerStart server
    return $ Server server cq ns ss cs bs conf

stopServer :: Server -> IO ()
-- TODO: Do method handles need to be freed?
stopServer Server{..} = do
  grpcDebug "stopServer: calling shutdownNotify."
  shutdownNotify
  grpcDebug "stopServer: cancelling all calls."
  C.grpcServerCancelAllCalls internalServer
  grpcDebug "stopServer: call grpc_server_destroy."
  C.grpcServerDestroy internalServer
  grpcDebug "stopServer: shutting down CQ."
  shutdownCQ

  where shutdownCQ = do
          shutdownResult <- shutdownCompletionQueue serverCQ
          case shutdownResult of
            Left _ -> do putStrLn "Warning: completion queue didn't shut down."
                         putStrLn "Trying to stop server anyway."
            Right _ -> return ()
        shutdownNotify = do
          let shutdownTag = C.tag 0
          serverShutdownAndNotify internalServer serverCQ shutdownTag
          grpcDebug "called serverShutdownAndNotify; plucking."
          shutdownEvent <- pluck serverCQ shutdownTag (Just 30)
          grpcDebug $ "shutdownNotify: got shutdown event" ++ show shutdownEvent
          case shutdownEvent of
            -- This case occurs when we pluck but the queue is already in the
            -- 'shuttingDown' state, implying we already tried to shut down.
            (Left GRPCIOShutdown) -> error "Called stopServer twice!"
            (Left _) -> error "Failed to stop server."
            (Right _) -> return ()

-- Uses 'bracket' to safely start and stop a server, even if exceptions occur.
withServer :: GRPC -> ServerConfig -> (Server -> IO a) -> IO a
withServer grpc cfg = bracket (startServer grpc cfg) stopServer

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
                     -> IO (RegisteredMethod mt)
serverRegisterMethod internalServer meth e mty =
  RegisteredMethod mty meth e <$> do
    h <- C.grpcServerRegisterMethod internalServer
           (unMethodName meth) (unEndpoint e) (payloadHandling mty)
    grpcDebug $ "registered method handle: " ++ show h ++ " of type " ++ show mty
    return h

-- | Create a 'Call' with which to wait for the invocation of a registered
-- method.
serverCreateCall :: Server
                 -> RegisteredMethod mt
                 -> IO (Either GRPCIOError ServerCall)
serverCreateCall Server{..} = serverRequestCall internalServer serverCQ

withServerCall :: Server
               -> RegisteredMethod mt
               -> (ServerCall -> IO (Either GRPCIOError a))
               -> IO (Either GRPCIOError a)
withServerCall server regmethod f = do
  createResult <- serverCreateCall server regmethod
  case createResult of
    Left x -> return $ Left x
    Right call -> f call `finally` logDestroy call
      where logDestroy c = grpcDebug "withServerRegisteredCall: destroying."
                           >> destroyServerCall c

--------------------------------------------------------------------------------
-- serverReader (server side of client streaming mode)

type ServerReaderHandler
  =  ServerCall
  -> StreamRecv
  -> Streaming (Maybe ByteString, MetadataMap, C.StatusCode, StatusDetails)

serverReader :: Server
             -> RegisteredMethod 'ClientStreaming
             -> MetadataMap -- ^ initial server metadata
             -> ServerReaderHandler
             -> IO (Either GRPCIOError ())
serverReader s@Server{ serverCQ = cq } rm initMeta f = withServerCall s rm go
  where
    go sc@(unServerCall -> c) = runExceptT $ do
      lift $ debugServerCall sc
      (mmsg, trailMD, st, ds) <-
        runStreamingProxy "serverReader" c cq (f sc streamRecv)
      runOps' c cq ( OpSendInitialMetadata initMeta
                   : OpSendStatusFromServer trailMD st ds
                   : maybe [] ((:[]) . OpSendMessage) mmsg
                   )
      return ()

--------------------------------------------------------------------------------
-- serverWriter (server side of server streaming mode)

type ServerWriterHandler
  =  ServerCall
  -> StreamSend
  -> Streaming (MetadataMap, C.StatusCode, StatusDetails)

-- | Wait for and then handle a registered, server-streaming call.
serverWriter :: Server
             -> RegisteredMethod 'ServerStreaming
             -> MetadataMap
             -- ^ Initial server metadata
             -> ServerWriterHandler
             -> IO (Either GRPCIOError ())
serverWriter s@Server{ serverCQ = cq } rm initMeta f = withServerCall s rm go
  where
    go sc@ServerCall{ unServerCall = c } = runExceptT $ do
      lift (debugServerCall sc)
      sendInitialMetadata c cq initMeta
      st <- runStreamingProxy "serverWriter" c cq (f sc streamSend)
      sendStatusFromServer c cq st

--------------------------------------------------------------------------------
-- serverRW (server side of bidirectional streaming mode)

type ServerRWHandler
  =  ServerCall
  -> StreamRecv
  -> StreamSend
  -> Streaming (MetadataMap, C.StatusCode, StatusDetails)

serverRW :: Server
         -> RegisteredMethod 'BiDiStreaming
         -> MetadataMap
            -- ^ initial server metadata
         -> ServerRWHandler
         -> IO (Either GRPCIOError ())
serverRW s@Server{ serverCQ = cq } rm initMeta f = withServerCall s rm go
  where
    go sc@(unServerCall -> c) = runExceptT $ do
      lift $ debugServerCall sc
      sendInitialMetadata c cq initMeta
      st <- runStreamingProxy "serverRW" c cq (f sc streamRecv streamSend)
      sendStatusFromServer c cq st

--------------------------------------------------------------------------------
-- serverHandleNormalCall (server side of normal request/response)

-- | A handler for a registered server call; bytestring parameter is request
-- body, with the bytestring response body in the result tuple. The first
-- metadata parameter refers to the request metadata, with the two metadata
-- values in the result tuple being the initial and trailing metadata
-- respectively. We pass in the 'ServerCall' so that the server can call
-- 'serverCallCancel' on it if needed.
type ServerHandler
  =  ServerCall -> ByteString -> MetadataMap
  -> IO (ByteString, MetadataMap, C.StatusCode, StatusDetails)

-- | Wait for and then handle a normal (non-streaming) call.
serverHandleNormalCall :: Server
                       -> RegisteredMethod 'Normal
                       -> MetadataMap
                       -- ^ Initial server metadata
                       -> ServerHandler
                       -> IO (Either GRPCIOError ())
serverHandleNormalCall s@Server{ serverCQ = cq } rm initMeta f =
  withServerCall s rm go
  where
    go sc@(unServerCall -> call) = do
      grpcDebug "serverHandleNormalCall(R): starting batch."
      debugServerCall sc
      case optionalPayload sc of
        Nothing  -> return (Left GRPCIOInternalMissingExpectedPayload)
        Just pay -> do
          (rspBody, trailMeta, status, ds) <- f sc pay (requestMetadataRecv sc)
          eea <- runOps call cq
                   [ OpSendInitialMetadata initMeta
                   , OpRecvCloseOnServer
                   , OpSendMessage rspBody
                   , OpSendStatusFromServer trailMeta status ds
                   ]
                 <* grpcDebug "serverHandleNormalCall(R): finished response ops."
          return (void eea)
