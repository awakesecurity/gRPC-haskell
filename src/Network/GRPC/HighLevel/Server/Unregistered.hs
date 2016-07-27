{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Network.GRPC.HighLevel.Server.Unregistered where

import           Control.Concurrent
import           Control.Concurrent.Async                  (async, wait)
import           Control.Arrow
import qualified Control.Exception                         as CE
import           Control.Monad
import           Data.Foldable                             (find)
import           Data.Protobuf.Wire.Class
import           Network.GRPC.HighLevel.Server
import           Network.GRPC.LowLevel
import qualified Network.GRPC.LowLevel.Call.Unregistered   as U
import qualified Network.GRPC.LowLevel.Server.Unregistered as U

dispatchLoop :: Server
              -> MetadataMap
              -> [Handler 'Normal]
              -> [Handler 'ClientStreaming]
              -> [Handler 'ServerStreaming]
              -> [Handler 'BiDiStreaming]
              -> IO ()
dispatchLoop server meta hN hC hS hB =
  forever $ U.withServerCallAsync server $ \call -> do
    case findHandler call allHandlers of
      Just (AnyHandler (UnaryHandler _ h)) -> unaryHandler call h
      Just (AnyHandler (ClientStreamHandler _ h)) -> csHandler call h
      Just (AnyHandler (ServerStreamHandler _ h)) -> ssHandler call h
      Just (AnyHandler (BiDiStreamHandler _ h)) -> bdHandler call h
      Nothing -> unknownHandler call
  where allHandlers = map AnyHandler hN
                      ++ map AnyHandler hC
                      ++ map AnyHandler hS
                      ++ map AnyHandler hB
        findHandler call = find ((== (U.callMethod call))
                                 . anyHandlerMethodName)
        unknownHandler call =
          void $ U.serverHandleNormalCall' server call meta $ \_ _ ->
            return (mempty
                    , mempty
                    , StatusNotFound
                    , StatusDetails "unknown method")

        handleError = (handleCallError . left herr =<<) . CE.try
          where herr (e :: CE.SomeException) = GRPCIOHandlerException (show e)

        unaryHandler :: (Message a, Message b) =>
                        U.ServerCall
                        -> ServerHandler a b
                        -> IO ()
        unaryHandler call h =
          handleError $
            U.serverHandleNormalCall' server call meta $ \_call' bs ->
              convertServerHandler h (fmap (const bs) $ U.convertCall call)
        csHandler :: (Message a, Message b) =>
                     U.ServerCall
                     -> ServerReaderHandler a b
                     -> IO ()
        csHandler call h =
          handleError $
            U.serverReader server call meta (convertServerReaderHandler h)
        ssHandler :: (Message a, Message b) =>
                     U.ServerCall
                     -> ServerWriterHandler a b
                     -> IO ()
        ssHandler call h =
          handleError $
            U.serverWriter server call meta (convertServerWriterHandler h)
        bdHandler :: (Message a, Message b) =>
                     U.ServerCall
                     -> ServerRWHandler a b
                     -> IO ()
        bdHandler call h =
          handleError $
            U.serverRW server call meta (convertServerRWHandler h)

serverLoop :: ServerOptions -> IO ()
serverLoop ServerOptions{..} = do
  -- We run the loop in a new thread so that we can kill the serverLoop thread.
  -- Without this fork, we block on a foreign call, which can't be interrupted.
  tid <- async $ withGRPC $ \grpc ->
    withServer grpc config $ \server -> do
      dispatchLoop server
                   optInitialMetadata
                   optNormalHandlers
                   optClientStreamHandlers
                   optServerStreamHandlers
                   optBiDiStreamHandlers
  wait tid
  where
    config =
      ServerConfig
        {  host = "localhost"
         , port = optServerPort
         , methodsToRegisterNormal = []
         , methodsToRegisterClientStreaming = []
         , methodsToRegisterServerStreaming = []
         , methodsToRegisterBiDiStreaming = []
         , serverArgs =
             ([CompressionAlgArg GrpcCompressDeflate | optUseCompression]
              ++
              [UserAgentPrefix optUserAgentPrefix
               , UserAgentSuffix optUserAgentSuffix])
        }
