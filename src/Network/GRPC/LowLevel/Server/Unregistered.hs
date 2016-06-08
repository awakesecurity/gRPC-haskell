{-# LANGUAGE RecordWildCards #-}

module Network.GRPC.LowLevel.Server.Unregistered where

import           Control.Exception                       (bracket, finally)
import           Control.Monad
import           Data.ByteString                         (ByteString)
import           Foreign.Ptr                             (nullPtr)
import qualified Network.GRPC.Unsafe                     as C
import qualified Network.GRPC.Unsafe.Op                  as C

import           Network.GRPC.LowLevel.Call
import           Network.GRPC.LowLevel.Call.Unregistered
import           Network.GRPC.LowLevel.CompletionQueue   (CompletionQueue,
                                                          TimeoutSeconds,
                                                          createCompletionQueue,
                                                          pluck,
                                                          serverRegisterCompletionQueue,
                                                          serverRequestCall,
                                                          serverRequestRegisteredCall,
                                                          serverShutdownAndNotify,
                                                          shutdownCompletionQueue)
import           Network.GRPC.LowLevel.GRPC
import           Network.GRPC.LowLevel.Op
import           Network.GRPC.LowLevel.Server

serverCreateUnregCall :: Server -> TimeoutSeconds
                    -> IO (Either GRPCIOError ServerUnregCall)
serverCreateUnregCall Server{..} timeLimit =
  serverRequestCall internalServer serverCQ timeLimit

withServerUnregCall :: Server -> TimeoutSeconds
                  -> (ServerUnregCall -> IO (Either GRPCIOError a))
                  -> IO (Either GRPCIOError a)
withServerUnregCall server timeout f = do
  createResult <- serverCreateUnregCall server timeout
  case createResult of
    Left x -> return $ Left x
    Right call -> f call `finally` logDestroy call
      where logDestroy c = grpcDebug "withServerCall: destroying."
                           >> destroyServerUnregCall c

--  TODO: This is preliminary.
-- We still need to provide the method name to the handler.
-- | Handle one unregistered call.
serverHandleNormalCall :: Server -> TimeoutSeconds
                          -> MetadataMap
                          -- ^ Initial server metadata.
                          -> (ByteString -> MetadataMap -> MethodName
                              -> IO (ByteString, MetadataMap, StatusDetails))
                          -- ^ Handler function takes a request body and
                          -- metadata and returns a response body and metadata.
                          -> IO (Either GRPCIOError ())
serverHandleNormalCall s@Server{..} timeLimit srvMetadata f = do
  withServerUnregCall s timeLimit $ \call -> do
    grpcDebug "serverHandleNormalCall: starting batch."
    let recvOps = serverOpsGetNormalCall srvMetadata
    opResults <- runServerUnregOps call serverCQ recvOps timeLimit
    case opResults of
      Left x -> do grpcDebug "serverHandleNormalCall: ops failed; aborting"
                   return $ Left x
      Right [OpRecvMessageResult (Just body)] -> do
        requestMeta <- serverUnregCallGetMetadata call
        grpcDebug $ "got client metadata: " ++ show requestMeta
        methodName <- serverUnregCallGetMethodName call
        hostName <- serverUnregCallGetHost call
        grpcDebug $ "call_details host is: " ++ show hostName
        (respBody, respMetadata, details) <- f body requestMeta methodName
        let status = C.GrpcStatusOk
        let respOps = serverOpsSendNormalResponse
                        respBody respMetadata status details
        respOpsResults <- runServerUnregOps call serverCQ respOps timeLimit
        case respOpsResults of
          Left x -> do grpcDebug "serverHandleNormalCall: resp failed."
                       return $ Left x
          Right _ -> grpcDebug "serverHandleNormalCall: ops done."
                     >> return (Right ())
      x -> error $ "impossible pattern match: " ++ show x
