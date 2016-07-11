{-# LANGUAGE RecordWildCards #-}

module Network.GRPC.LowLevel.CompletionQueue.Unregistered where

import           Control.Exception                              (bracket)
import           Foreign.Marshal.Alloc                          (free, malloc)
import           Foreign.Storable                               (peek)
import           Network.GRPC.LowLevel.Call
import qualified Network.GRPC.LowLevel.Call.Unregistered        as U
import           Network.GRPC.LowLevel.CompletionQueue.Internal
import           Network.GRPC.LowLevel.GRPC
import qualified Network.GRPC.Unsafe                            as C
import qualified Network.GRPC.Unsafe.Constants                  as C
import qualified Network.GRPC.Unsafe.Metadata                   as C
import qualified Network.GRPC.Unsafe.Time                       as C

channelCreateCall :: C.Channel
                  -> C.Call
                  -> C.PropagationMask
                  -> CompletionQueue
                  -> MethodName
                  -> Endpoint
                  -> C.CTimeSpecPtr
                  -> IO (Either GRPCIOError ClientCall)
channelCreateCall chan parent mask cq@CompletionQueue{..} meth endpt deadline =
  withPermission Push cq $ do
    call <- C.grpcChannelCreateCall chan parent mask unsafeCQ
              (unMethodName meth) (unEndpoint endpt) deadline C.reserved
    return $ Right $ ClientCall call


serverRequestCall :: C.Server
                  -> CompletionQueue
                  -> IO (Either GRPCIOError U.ServerCall)
serverRequestCall server cq@CompletionQueue{..} =
  withPermission Push cq $
    bracket malloc free $ \callPtr ->
      C.withMetadataArrayPtr $ \metadataArrayPtr ->
        C.withCallDetails $ \callDetails ->
          withPermission Pluck cq $ do
            grpcDebug $ "serverRequestCall: callPtr is " ++ show callPtr
            metadataArray <- peek metadataArrayPtr
            tag <- newTag cq
            callError <- C.grpcServerRequestCall server callPtr callDetails
                           metadataArray unsafeCQ unsafeCQ tag
            grpcDebug $ "serverRequestCall: callError was " ++ show callError
            if callError /= C.CallOk
               then do grpcDebug "serverRequestCall: got call error; cleaning up."
                       return $ Left $ GRPCIOCallError callError
               else do pluckResult <- pluck cq tag Nothing
                       grpcDebug $ "serverRequestCall: pluckResult was "
                                   ++ show pluckResult
                       case pluckResult of
                         Left x -> do
                           grpcDebug "serverRequestCall: pluck error."
                           return $ Left x
                         Right () -> do
                           rawCall <- peek callPtr
                           metadata <- C.getAllMetadataArray metadataArray
                           deadline <- getDeadline callDetails
                           method <- getMethod callDetails
                           host <- getHost callDetails
                           let call = U.ServerCall rawCall
                                                   metadata
                                                   Nothing
                                                   deadline
                                                   method
                                                   host
                           return $ Right call

      where getDeadline callDetails = do
              C.timeSpec <$> C.callDetailsGetDeadline callDetails
            getMethod callDetails =
              MethodName <$> C.callDetailsGetMethod callDetails
            getHost callDetails =
              Host <$> C.callDetailsGetHost callDetails
