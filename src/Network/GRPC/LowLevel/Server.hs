{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE PatternSynonyms     #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE TupleSections       #-}
{-# LANGUAGE ViewPatterns        #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | This module defines data structures and operations pertaining to registered
-- servers using registered calls; for unregistered support, see
-- `Network.GRPC.LowLevel.Server.Unregistered`.
module Network.GRPC.LowLevel.Server where

import           Control.Exception                     (bracket, finally)
import           Control.Monad
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

-- | Wraps various gRPC state needed to run a server.
data Server = Server
  { serverGRPC           :: GRPC
  , unsafeServer         :: C.Server
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
  , methodsToRegisterNormal :: [MethodName]
    -- ^ List of normal (non-streaming) methods to register.
  , methodsToRegisterClientStreaming :: [MethodName]
  , methodsToRegisterServerStreaming :: [MethodName]
  , methodsToRegisterBiDiStreaming :: [MethodName]
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
    grpcDebug $ "startServer: server CQ: " ++ show cq
    serverRegisterCompletionQueue server cq

    -- Register methods according to their GRPCMethodType kind. It's a bit ugly
    -- to partition them this way, but we get very convenient phantom typing
    -- elsewhere by doing so.
    -- TODO: change order of args so we can eta reduce.
    ns <- mapM (\nm -> serverRegisterMethodNormal server nm e)
               methodsToRegisterNormal
    ss <- mapM (\nm -> serverRegisterMethodServerStreaming server nm e)
               methodsToRegisterServerStreaming
    cs <- mapM (\nm -> serverRegisterMethodClientStreaming server nm e)
               methodsToRegisterClientStreaming
    bs <- mapM (\nm -> serverRegisterMethodBiDiStreaming server nm e)
               methodsToRegisterBiDiStreaming
    C.grpcServerStart server
    return $ Server grpc server cq ns ss cs bs conf

stopServer :: Server -> IO ()
-- TODO: Do method handles need to be freed?
stopServer Server{ unsafeServer = s, serverCQ = scq } = do
  grpcDebug "stopServer: calling shutdownNotify."
  shutdownNotify
  grpcDebug "stopServer: cancelling all calls."
  C.grpcServerCancelAllCalls s
  grpcDebug "stopServer: call grpc_server_destroy."
  C.grpcServerDestroy s
  grpcDebug "stopServer: shutting down CQ."
  shutdownCQ

  where shutdownCQ = do
          shutdownResult <- shutdownCompletionQueue scq
          case shutdownResult of
            Left _ -> do putStrLn "Warning: completion queue didn't shut down."
                         putStrLn "Trying to stop server anyway."
            Right _ -> return ()
        shutdownNotify = do
          let shutdownTag = C.tag 0
          serverShutdownAndNotify s scq shutdownTag
          grpcDebug "called serverShutdownAndNotify; plucking."
          shutdownEvent <- pluck scq shutdownTag (Just 30)
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

-- | Less precisely-typed registration function used in
-- 'serverRegisterMethodNormal', 'serverRegisterMethodServerStreaming',
-- 'serverRegisterMethodClientStreaming', and
-- 'serverRegisterMethodBiDiStreaming'.
serverRegisterMethod :: C.Server
                        -> MethodName
                        -> Endpoint
                        -> GRPCMethodType
                        -> IO (C.CallHandle)
serverRegisterMethod s nm e mty =
  C.grpcServerRegisterMethod s
                             (unMethodName nm)
                             (unEndpoint e)
                             (payloadHandling mty)

{-
TODO: Consolidate the register functions below.

It seems like we'd need true dependent types to use only one
  registration function. Ideally we'd want a type like
  serverRegisterMethod :: C.Server
                          -> MethodName
                          -> Endpoint
                          -> (t :: GRPCMethodType)
                          -> IO (RegisteredMethod (Lifted t))

where `Lifted t` is the type in the t data kind that corresponds to the data
constructor t the function was given.

-}

-- | Register a method on a server. The 'RegisteredMethod' type can then be used
-- to wait for a request to arrive. Note: gRPC claims this must be called before
-- the server is started, so we do it during startup according to the
-- 'ServerConfig'.
serverRegisterMethodNormal :: C.Server
                     -> MethodName
                     -- ^ method name, e.g. "/foo"
                     -> Endpoint
                     -- ^ Endpoint name name, e.g. "localhost:9999". I have no
                     -- idea why this is needed since we have to provide these
                     -- parameters to start a server in the first place. It
                     -- doesn't seem to have any effect, even if it's filled
                     -- with nonsense.
                     -> IO (RegisteredMethod 'Normal)
serverRegisterMethodNormal internalServer meth e = do
  h <- serverRegisterMethod internalServer meth e Normal
  return $ RegisteredMethodNormal meth e h

serverRegisterMethodClientStreaming
  :: C.Server
     -> MethodName
     -- ^ method name, e.g. "/foo"
     -> Endpoint
     -- ^ Endpoint name name, e.g. "localhost:9999". I have no
     -- idea why this is needed since we have to provide these
     -- parameters to start a server in the first place. It
     -- doesn't seem to have any effect, even if it's filled
     -- with nonsense.
     -> IO (RegisteredMethod 'ClientStreaming)
serverRegisterMethodClientStreaming internalServer meth e = do
  h <- serverRegisterMethod internalServer meth e ClientStreaming
  return $ RegisteredMethodClientStreaming meth e h


serverRegisterMethodServerStreaming
  :: C.Server
     -> MethodName
     -- ^ method name, e.g. "/foo"
     -> Endpoint
     -- ^ Endpoint name name, e.g. "localhost:9999". I have no
     -- idea why this is needed since we have to provide these
     -- parameters to start a server in the first place. It
     -- doesn't seem to have any effect, even if it's filled
     -- with nonsense.
     -> IO (RegisteredMethod 'ServerStreaming)
serverRegisterMethodServerStreaming internalServer meth e = do
  h <- serverRegisterMethod internalServer meth e ServerStreaming
  return $ RegisteredMethodServerStreaming meth e h


serverRegisterMethodBiDiStreaming
  :: C.Server
     -> MethodName
     -- ^ method name, e.g. "/foo"
     -> Endpoint
     -- ^ Endpoint name name, e.g. "localhost:9999". I have no
     -- idea why this is needed since we have to provide these
     -- parameters to start a server in the first place. It
     -- doesn't seem to have any effect, even if it's filled
     -- with nonsense.
     -> IO (RegisteredMethod 'BiDiStreaming)
serverRegisterMethodBiDiStreaming internalServer meth e = do
  h <- serverRegisterMethod internalServer meth e BiDiStreaming
  return $ RegisteredMethodBiDiStreaming meth e h

-- | Create a 'Call' with which to wait for the invocation of a registered
-- method.
serverCreateCall :: Server
                 -> RegisteredMethod mt
                 -> IO (Either GRPCIOError (ServerCall (MethodPayload mt)))
serverCreateCall Server{..} rm = do
  callCQ <- createCompletionQueue serverGRPC
  serverRequestCall rm unsafeServer serverCQ callCQ

withServerCall :: Server
               -> RegisteredMethod mt
               -> (ServerCall (MethodPayload mt) -> IO (Either GRPCIOError a))
               -> IO (Either GRPCIOError a)
withServerCall s rm f =
    serverCreateCall s rm >>= \case
      Left e  -> return (Left e)
      Right c -> do
        debugServerCall c
        f c `finally` do
          grpcDebug "withServerCall(R): destroying."
          destroyServerCall c

--------------------------------------------------------------------------------
-- serverReader (server side of client streaming mode)

type ServerReaderHandlerLL
  =  ServerCall ()
  -> StreamRecv ByteString
  -> Streaming (Maybe ByteString, MetadataMap, C.StatusCode, StatusDetails)

serverReader :: Server
             -> RegisteredMethod 'ClientStreaming
             -> MetadataMap -- ^ initial server metadata
             -> ServerReaderHandlerLL
             -> IO (Either GRPCIOError ())
serverReader s rm initMeta f = withServerCall s rm go
  where
    go sc@ServerCall{ unsafeSC = c, callCQ = ccq } = runExceptT $ do
      (mmsg, trailMeta, st, ds) <-
        runStreamingProxy "serverReader" c ccq (f sc streamRecv)
      runOps' c ccq ( OpSendInitialMetadata initMeta
                    : OpSendStatusFromServer trailMeta st ds
                    : maybe [] ((:[]) . OpSendMessage) mmsg
                    )
      return ()

--------------------------------------------------------------------------------
-- serverWriter (server side of server streaming mode)

type ServerWriterHandlerLL
  =  ServerCall ByteString
  -> StreamSend ByteString
  -> Streaming (MetadataMap, C.StatusCode, StatusDetails)

-- | Wait for and then handle a registered, server-streaming call.
serverWriter :: Server
             -> RegisteredMethod 'ServerStreaming
             -> MetadataMap
             -- ^ Initial server metadata
             -> ServerWriterHandlerLL
             -> IO (Either GRPCIOError ())
serverWriter s rm initMeta f = withServerCall s rm go
  where
    go sc@ServerCall{ unsafeSC = c, callCQ = ccq } = runExceptT $ do
      sendInitialMetadata c ccq initMeta
      st <- runStreamingProxy "serverWriter" c ccq (f sc streamSend)
      sendStatusFromServer c ccq st

--------------------------------------------------------------------------------
-- serverRW (server side of bidirectional streaming mode)

type ServerRWHandlerLL
  =  ServerCall ()
  -> StreamRecv ByteString
  -> StreamSend ByteString
  -> Streaming (MetadataMap, C.StatusCode, StatusDetails)

serverRW :: Server
         -> RegisteredMethod 'BiDiStreaming
         -> MetadataMap
            -- ^ initial server metadata
         -> ServerRWHandlerLL
         -> IO (Either GRPCIOError ())
serverRW s rm initMeta f = withServerCall s rm go
  where
    go sc@ServerCall{ unsafeSC = c, callCQ = ccq } = runExceptT $ do
      sendInitialMetadata c ccq initMeta
      st <- runStreamingProxy "serverRW" c ccq (f sc streamRecv streamSend)
      sendStatusFromServer c ccq st

--------------------------------------------------------------------------------
-- serverHandleNormalCall (server side of normal request/response)

-- | A handler for a registered server call; bytestring parameter is request
-- body, with the bytestring response body in the result tuple. The first
-- metadata parameter refers to the request metadata, with the two metadata
-- values in the result tuple being the initial and trailing metadata
-- respectively. We pass in the 'ServerCall' so that the server can call
-- 'serverCallCancel' on it if needed.
type ServerHandlerLL
  =  ServerCall ByteString
  -> IO (ByteString, MetadataMap, C.StatusCode, StatusDetails)

-- | Wait for and then handle a normal (non-streaming) call.
serverHandleNormalCall :: Server
                       -> RegisteredMethod 'Normal
                       -> MetadataMap
                       -- ^ Initial server metadata
                       -> ServerHandlerLL
                       -> IO (Either GRPCIOError ())
serverHandleNormalCall s rm initMeta f =
  withServerCall s rm go
  where
    go sc@ServerCall{..} = do
      (rsp, trailMeta, st, ds) <- f sc
      void <$> runOps unsafeSC callCQ
                 [ OpSendInitialMetadata initMeta
                 , OpRecvCloseOnServer
                 , OpSendMessage rsp
                 , OpSendStatusFromServer trailMeta st ds
                 ]
               <* grpcDebug "serverHandleNormalCall(R): finished response ops."
