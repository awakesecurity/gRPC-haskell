{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Network.GRPC.HighLevel.Server.Unregistered where

import Control.Arrow
import Control.Concurrent.MVar (
  newEmptyMVar,
  putMVar,
  takeMVar,
 )
import qualified Control.Exception as CE
import Control.Monad
import Data.Foldable (find)
import Network.GRPC.HighLevel.Server
import Network.GRPC.LowLevel
import qualified Network.GRPC.LowLevel.Call.Unregistered as U
import Network.GRPC.LowLevel.Server (forkServer)
import qualified Network.GRPC.LowLevel.Server.Unregistered as U
import Proto3.Suite.Class

dispatchLoop ::
  Server ->
  (String -> IO ()) ->
  MetadataMap ->
  [Handler 'Normal] ->
  [Handler 'ClientStreaming] ->
  [Handler 'ServerStreaming] ->
  [Handler 'BiDiStreaming] ->
  IO ()
dispatchLoop s logger md hN hC hS hB = go
  where
    -- Once the server begins shutting down, every request for a new call fails
    -- immediately, so stop asking rather than spin.
    go =
      U.withServerCallAsync' s dispatch >>= \case
        Left GRPCIOShutdown -> return ()
        _ -> go

    dispatch sc =
      case findHandler sc allHandlers of
        Just (AnyHandler ah) -> case ah of
          UnaryHandler _ h -> unaryHandler sc h
          ClientStreamHandler _ h -> csHandler sc h
          ServerStreamHandler _ h -> ssHandler sc h
          BiDiStreamHandler _ h -> bdHandler sc h
        Nothing -> unknownHandler sc

    allHandlers =
      map AnyHandler hN
        ++ map AnyHandler hC
        ++ map AnyHandler hS
        ++ map AnyHandler hB

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
      where
        herr (e :: CE.SomeException) = GRPCIOHandlerException (show e)

serverLoop :: ServerOptions -> IO ()
serverLoop ServerOptions{..} =
  -- "dispatchLoop" runs in its own thread, registered with the server
  -- via "forkServer", while the "serverLoop" thread sleeps in "takeMVar".
  --
  -- This structure dates from when "dispatchLoop" waited for calls in an
  -- uninterruptible foreign call, so that only a GRPC shutdown could
  -- wake it. Waiting for a call is now an interruptible "takeMVar" (see
  -- "Network.GRPC.LowLevel.CompletionQueue.Internal"), and "dispatchLoop"
  -- also returns by itself once the shutdown begins.
  --
  -- The "withServer" cleanup code initiates the GRPC shutdown.
  withGRPC $ \grpc ->
    withServer grpc config $ \server -> do
      -- Killing the "serverLoop" thread triggers the "withServer"
      -- cleanup code, which initiates a shutdown, which in turn
      -- kills the "dispatchLoop" thread and any other thread we
      -- may have started with "forkServer".
      done <- newEmptyMVar
      launched <-
        forkServer server $
          dispatchLoop
            server
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
    config =
      ServerConfig
        { host = optServerHost
        , port = optServerPort
        , methodsToRegisterNormal = []
        , methodsToRegisterClientStreaming = []
        , methodsToRegisterServerStreaming = []
        , methodsToRegisterBiDiStreaming = []
        , serverArgs =
            [CompressionAlgArg GrpcCompressDeflate | optUseCompression]
              ++ [ UserAgentPrefix optUserAgentPrefix
                 , UserAgentSuffix optUserAgentSuffix
                 ]
              ++ foldMap (pure . MaxReceiveMessageLength) optMaxReceiveMessageLength
              ++ foldMap (pure . MaxMetadataSize) optMaxMetadataSize
        , sslConfig = optSSLConfig
        }
