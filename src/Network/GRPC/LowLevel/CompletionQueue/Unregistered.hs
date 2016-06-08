{-# LANGUAGE RecordWildCards #-}

module Network.GRPC.LowLevel.CompletionQueue.Unregistered where

import           Control.Concurrent                             (forkIO)
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


serverRequestCall :: C.Server -> CompletionQueue -> TimeoutSeconds
                     -> IO (Either GRPCIOError U.ServerCall)
serverRequestCall server cq@CompletionQueue{..} timeLimit =
  withPermission Push cq $ do
    callPtr <- malloc
    grpcDebug $ "serverRequestCall: callPtr is " ++ show callPtr
    callDetails <- C.createCallDetails
    metadataArrayPtr <- C.metadataArrayCreate
    metadataArray <- peek metadataArrayPtr
    tag <- newTag cq
    callError <- C.grpcServerRequestCall server callPtr callDetails
                   metadataArray unsafeCQ unsafeCQ tag
    grpcDebug $ "serverRequestCall: callError was " ++ show callError
    if callError /= C.CallOk
       then do grpcDebug "serverRequestCall: got call error; cleaning up."
               failureCleanup callPtr callDetails metadataArrayPtr
               return $ Left $ GRPCIOCallError callError
       else do pluckResult <- pluck cq tag timeLimit
               grpcDebug $ "serverRequestCall: pluckResult was "
                           ++ show pluckResult
               case pluckResult of
                 Left x -> do
                   grpcDebug "serverRequestCall: pluck error; cleaning up."
                   failureCleanup callPtr callDetails
                                  metadataArrayPtr
                   return $ Left x
                 Right () -> do
                   rawCall <- peek callPtr
                   let call = U.ServerCall rawCall
                                           metadataArrayPtr
                                           Nothing
                                           callDetails
                   return $ Right call

      --TODO: the gRPC library appears to hold onto these pointers for a random
      -- amount of time, even after returning from the only call that uses them.
      -- This results in malloc errors if
      -- gRPC tries to modify them after we free them. To work around it,
      -- we sleep for a while before freeing the objects. We should find a
      -- permanent solution that's more robust.
      where failureCleanup callPtr callDetails metadataArrayPtr = forkIO $ do
              threadDelaySecs 30
              grpcDebug "serverRequestCall: doing delayed cleanup."
              free callPtr
              C.destroyCallDetails callDetails
              C.metadataArrayDestroy metadataArrayPtr
              return ()
