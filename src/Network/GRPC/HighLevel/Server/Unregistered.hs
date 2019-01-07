{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Network.GRPC.HighLevel.Server.Unregistered where

import           Control.Arrow
import           Control.Concurrent.MVar                   (newEmptyMVar,
                                                            putMVar,
                                                            takeMVar)
import qualified Control.Exception                         as CE
import           Control.Monad
import           Data.Foldable                             (find)
import           Network.GRPC.HighLevel.Server
import           Network.GRPC.LowLevel
import           Network.GRPC.LowLevel.Server              (forkServer)
import qualified Network.GRPC.LowLevel.Call.Unregistered   as U
import qualified Network.GRPC.LowLevel.Server.Unregistered as U
import           Proto3.Suite.Class

dispatchLoop :: Server
             -> (String -> IO ())
             -> MetadataMap
             -> [Handler 'Normal]
             -> [Handler 'ClientStreaming]
             -> [Handler 'ServerStreaming]
             -> [Handler 'BiDiStreaming]
             -> IO ()
dispatchLoop s logger md hN hC hS hB =
  forever $ U.withServerCallAsync s $ \sc ->
    case findHandler sc allHandlers of
      Just (AnyHandler ah) -> case ah of
        UnaryHandler _ h        -> unaryHandler sc h
        ClientStreamHandler _ h -> csHandler sc h
        ServerStreamHandler _ h -> ssHandler sc h
        BiDiStreamHandler _ h   -> bdHandler sc h
      Nothing                   -> unknownHandler sc
  where
    allHandlers = map AnyHandler hN ++ map AnyHandler hC
                  ++ map AnyHandler hS ++ map AnyHandler hB

    findHandler sc = find ((== U.callMethod sc) . anyHandlerMethodName)

    unaryHandler :: (Message a, Message b) => U.ServerCall -> ServerHandler a b -> IO ()
    unaryHandler sc h =
      handleError $
        U.serverHandleNormalCall' s sc md $ \_sc' bs ->
          convertServerHandler h (const bs <$> U.convertCall sc)

    csHandler :: (Message a, Message b) => U.ServerCall -> ServerReaderHandler a b -> IO ()
    csHandler sc = handleError . U.serverReader s sc md . convertServerReaderHandler

    ssHandler :: (Message a, Message b) => U.ServerCall -> ServerWriterHandler a b -> IO ()
    ssHandler sc = handleError . U.serverWriter s sc md . convertServerWriterHandler

    bdHandler :: (Message a, Message b) => U.ServerCall -> ServerRWHandler a b -> IO ()
    bdHandler sc = handleError . U.serverRW s sc md . convertServerRWHandler

    unknownHandler :: U.ServerCall -> IO ()
    unknownHandler sc = void $ U.serverHandleNormalCall' s sc md $ \_ _ ->
      return (mempty, mempty, StatusNotFound, StatusDetails "unknown method")

    handleError :: IO a -> IO ()
    handleError = (handleCallError logger . left herr =<<) . CE.try
      where herr (e :: CE.SomeException) = GRPCIOHandlerException (show e)

serverLoop :: ServerOptions -> IO ()
serverLoop ServerOptions{..} =
  -- In the GRPC library, "doc/core/epoll-polling-engine.md" seems
  -- to indicate that the thread which actually awakens from sleep
  -- on file descriptor events may differ from the one which seeks
  -- to "pluck" the resulting event.
  --
  -- Thus it seems possible that "dispatchLoop" may be waiting on
  -- a condition variable when the "serverLoop" thread is killed.
  --
  -- Note that "pthread_cond_timedwait" never returns EINTR; see:
  -- <https://pubs.opengroup.org/onlinepubs/7908799/xsh/pthread_cond_wait.html>
  --
  -- Therefore to awaken "dispatchLoop" we must initiate a GRPC
  -- shutdown; it would not suffice to kill its Haskell thread.
  -- (Presumably a GRPC shutdown broadcasts on relvant condition
  -- variables; regardless, we do see it awaken "dispatchLoop".)
  --
  -- The "withServer" cleanup code will initiate a GRPC shutdown.
  -- We arrange to trigger it by leaving the "serverLoop" thread
  -- in an interruptible sleep ("takeMVar") while "dispatchLoop"
  -- runs in its own thread.
  withGRPC $ \grpc ->
    withServer grpc config $ \server -> do
      -- Killing the "serverLoop" thread triggers the "withServer"
      -- cleanup code, which initiates a shutdown, which in turn
      -- kills the "dispatchLoop" thread and any other thread we
      -- may have started with "forkServer".
      done <- newEmptyMVar
      launched <- forkServer server $
        dispatchLoop server
                     optLogger
                     optInitialMetadata
                     optNormalHandlers
                     optClientStreamHandlers
                     optServerStreamHandlers
                     optBiDiStreamHandlers
        `CE.finally` putMVar done ()
      when launched $
        takeMVar done
  where
    config = ServerConfig
      { host                             = optServerHost
      , port                             = optServerPort
      , methodsToRegisterNormal          = []
      , methodsToRegisterClientStreaming = []
      , methodsToRegisterServerStreaming = []
      , methodsToRegisterBiDiStreaming   = []
      , serverArgs                       =
          [CompressionAlgArg GrpcCompressDeflate | optUseCompression]
          ++
          [ UserAgentPrefix optUserAgentPrefix
          , UserAgentSuffix optUserAgentSuffix
          ]
      , sslConfig = optSSLConfig
      }
