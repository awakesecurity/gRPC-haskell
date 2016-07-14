{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE RecordWildCards   #-}

module Network.GRPC.HighLevel.Server.Unregistered where

import           Control.Monad
import           Data.Protobuf.Wire.Class
import           Data.Foldable                             (find)
import           Network.GRPC.HighLevel.Server
import           Network.GRPC.LowLevel
import qualified Network.GRPC.LowLevel.Server.Unregistered as U
import qualified Network.GRPC.LowLevel.Call.Unregistered as U

dispatchLoop :: Server
              -> [Handler 'Normal]
              -> [Handler 'ClientStreaming]
              -> [Handler 'ServerStreaming]
              -> [Handler 'BiDiStreaming]
              -> IO ()
dispatchLoop server hN hC hS hB =
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
          void $ U.serverHandleNormalCall' server call mempty $ \_ _ ->
            return (mempty
                    , mempty
                    , StatusNotFound
                    , StatusDetails "unknown method")
        handleError f = f >>= handleCallError
        unaryHandler :: (Message a, Message b) =>
                        U.ServerCall
                        -> ServerHandler a b
                        -> IO ()
        unaryHandler call h =
          handleError $
            U.serverHandleNormalCall' server call mempty $ \_call' bs ->
              convertServerHandler h (fmap (const bs) $ U.convertCall call)
        csHandler :: (Message a, Message b) =>
                     U.ServerCall
                     -> ServerReaderHandler a b
                     -> IO ()
        csHandler call h =
          handleError $
            U.serverReader server call mempty (convertServerReaderHandler h)
        ssHandler :: (Message a, Message b) =>
                     U.ServerCall
                     -> ServerWriterHandler a b
                     -> IO ()
        ssHandler call h =
          handleError $
            U.serverWriter server call mempty (convertServerWriterHandler h)
        bdHandler :: (Message a, Message b) =>
                     U.ServerCall
                     -> ServerRWHandler a b
                     -> IO ()
        bdHandler call h =
          handleError $
            U.serverRW server call mempty (convertServerRWHandler h)

serverLoop :: ServerOptions -> IO ()
serverLoop ServerOptions{..} =
  withGRPC $ \grpc ->
    withServer grpc config $ \server -> do
      dispatchLoop server
                   optNormalHandlers
                   optClientStreamHandlers
                   optServerStreamHandlers
                   optBiDiStreamHandlers
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
