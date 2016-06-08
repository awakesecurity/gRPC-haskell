{-# LANGUAGE RecordWildCards #-}

module Network.GRPC.LowLevel.Client.Unregistered where

import           Control.Exception                                  (finally)
import           Control.Monad                                      (join)
import           Data.ByteString                                    (ByteString)
import           Foreign.Ptr                                        (nullPtr)
import qualified Network.GRPC.Unsafe                                as C
import qualified Network.GRPC.Unsafe.Constants                      as C
import qualified Network.GRPC.Unsafe.Time                           as C

import           Network.GRPC.LowLevel.Call
import           Network.GRPC.LowLevel.Client
import           Network.GRPC.LowLevel.CompletionQueue              (TimeoutSeconds)
import qualified Network.GRPC.LowLevel.CompletionQueue.Unregistered as U
import           Network.GRPC.LowLevel.GRPC
import           Network.GRPC.LowLevel.Op

-- | Create a call on the client for an endpoint without using the
-- method registration machinery. In practice, we'll probably only use the
-- registered method version, but we include this for completeness and testing.
clientCreateCall :: Client
                 -> MethodName
                 -> TimeoutSeconds
                 -> IO (Either GRPCIOError ClientCall)
clientCreateCall Client{..} meth timeout = do
  let parentCall = C.Call nullPtr
  C.withDeadlineSeconds timeout $ \deadline -> do
    U.channelCreateCall clientChannel parentCall C.propagateDefaults
      clientCQ meth (clientEndpoint clientConfig) deadline

withClientCall :: Client
               -> MethodName
               -> TimeoutSeconds
               -> (ClientCall -> IO (Either GRPCIOError a))
               -> IO (Either GRPCIOError a)
withClientCall client method timeout f = do
  createResult <- clientCreateCall client method timeout
  case createResult of
    Left x -> return $ Left x
    Right call -> f call `finally` logDestroy call
                    where logDestroy c = grpcDebug "withClientCall: destroying."
                                         >> destroyClientCall c

-- | Makes a normal (non-streaming) request without needing to register a method
-- first. Probably only useful for testing. TODO: This is preliminary, like
-- 'clientRegisteredRequest'.
clientRequest :: Client
                 -> MethodName
                 -- ^ Method name, e.g. "/foo"
                 -> TimeoutSeconds
                 -- ^ "Number of seconds until request times out"
                 -> ByteString
                 -- ^ Request body.
                 -> MetadataMap
                 -- ^ Request metadata.
                 -> IO (Either GRPCIOError NormalRequestResult)
clientRequest client@Client{..} meth timeLimit body meta =
  fmap join $ do
  withClientCall client meth timeLimit $ \call -> do
    let ops = clientNormalRequestOps body meta
    results <- runClientOps call clientCQ ops timeLimit
    grpcDebug "clientRequest: ops ran."
    case results of
      Left x -> return $ Left x
      Right rs -> return $ Right $ compileNormalRequestResults rs
