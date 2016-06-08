-- | Unlike most of the other internal low-level modules, we don't export
-- everything here. There are several things in here that, if accessed, could
-- cause race conditions, so we only expose functions that are thread safe.
-- However, some of the functions we export here can cause memory leaks if used
-- improperly.
--
-- When definition operations which pertain to calls, this module only provides
-- definitions for registered calls; for unregistered variants, see
-- `Network.GRPC.LowLevel.CompletionQueue.Unregistered`. Type definitions and
-- implementation details to both are kept in
-- `Network.GRPC.LowLevel.CompletionQueue.Internal`.

{-# LANGUAGE RecordWildCards #-}

module Network.GRPC.LowLevel.CompletionQueue
  ( CompletionQueue
  , withCompletionQueue
  , createCompletionQueue
  , shutdownCompletionQueue
  , pluck
  , startBatch
  , channelCreateCall
  , TimeoutSeconds
  , isEventSuccessful
  , serverRegisterCompletionQueue
  , serverShutdownAndNotify
  , serverRequestCall
  , newTag
  )
where

import           Control.Concurrent                      (forkIO)
import           Control.Concurrent.STM                  (atomically, check)
import           Control.Concurrent.STM.TVar             (newTVarIO, readTVar,
                                                          writeTVar)
import           Control.Exception                       (bracket)
import           Data.IORef                              (newIORef)
import           Data.List                               (intersperse)
import           Foreign.Marshal.Alloc                   (free, malloc)
import           Foreign.Storable                        (peek)
import qualified Network.GRPC.Unsafe                     as C
import qualified Network.GRPC.Unsafe.Constants           as C
import qualified Network.GRPC.Unsafe.Metadata            as C
import qualified Network.GRPC.Unsafe.Op                  as C
import qualified Network.GRPC.Unsafe.Time                as C
import           System.Timeout                          (timeout)

import           Network.GRPC.LowLevel.Call
import           Network.GRPC.LowLevel.GRPC
import           Network.GRPC.LowLevel.CompletionQueue.Internal

withCompletionQueue :: GRPC -> (CompletionQueue -> IO a) -> IO a
withCompletionQueue grpc = bracket (createCompletionQueue grpc)
                                   shutdownCompletionQueue

createCompletionQueue :: GRPC -> IO CompletionQueue
createCompletionQueue _ = do
  unsafeCQ <- C.grpcCompletionQueueCreate C.reserved
  currentPluckers <- newTVarIO 0
  currentPushers <- newTVarIO 0
  shuttingDown <- newTVarIO False
  nextTag <- newIORef minBound
  return $ CompletionQueue{..}

-- TODO: I'm thinking it might be easier to use 'Either' uniformly everywhere
-- even when it's isomorphic to 'Maybe'. If that doesn't turn out to be the
-- case, switch these to 'Maybe'.
-- | Very simple wrapper around 'grpcCallStartBatch'. Throws 'GRPCIOShutdown'
-- without calling 'grpcCallStartBatch' if the queue is shutting down.
-- Throws 'CallError' if 'grpcCallStartBatch' returns a non-OK code.
startBatch :: CompletionQueue -> C.Call -> C.OpArray -> Int -> C.Tag
              -> IO (Either GRPCIOError ())
startBatch cq@CompletionQueue{..} call opArray opArraySize tag =
    withPermission Push cq $ fmap throwIfCallError $ do
      grpcDebug $ "startBatch: calling grpc_call_start_batch with pointers: "
                  ++ show call ++ " " ++ show opArray
      res <- C.grpcCallStartBatch call opArray opArraySize tag C.reserved
      grpcDebug "startBatch: grpc_call_start_batch call returned."
      return res


-- | Shuts down the completion queue. See the comment above 'CompletionQueue'
-- for the strategy we use to ensure that no one tries to use the
-- queue after we begin the shutdown process. Errors with
-- 'GRPCIOShutdownFailure' if the queue can't be shut down within 5 seconds.
shutdownCompletionQueue :: CompletionQueue -> IO (Either GRPCIOError ())
shutdownCompletionQueue (CompletionQueue{..}) = do
  atomically $ writeTVar shuttingDown True
  atomically $ readTVar currentPushers >>= \x -> check (x == 0)
  atomically $ readTVar currentPluckers >>= \x -> check (x == 0)
  --drain the queue
  C.grpcCompletionQueueShutdown unsafeCQ
  loopRes <- timeout (5*10^(6::Int)) drainLoop
  case loopRes of
    Nothing -> return $ Left GRPCIOShutdownFailure
    Just () -> C.grpcCompletionQueueDestroy unsafeCQ >> return (Right ())

  where drainLoop :: IO ()
        drainLoop = do
          deadline <- C.secondsToDeadline 1
          ev <- C.grpcCompletionQueueNext unsafeCQ deadline C.reserved
          case (C.eventCompletionType ev) of
            C.QueueShutdown -> return ()
            C.QueueTimeout -> drainLoop
            C.OpComplete -> drainLoop

channelCreateCall :: C.Channel
                  -> C.Call
                  -> C.PropagationMask
                  -> CompletionQueue
                  -> C.CallHandle
                  -> C.CTimeSpecPtr
                  -> IO (Either GRPCIOError ClientCall)
channelCreateCall
  chan parent mask cq@CompletionQueue{..} handle deadline =
  withPermission Push cq $ do
    grpcDebug $ "channelCreateCall: call with "
                ++ concat (intersperse " " [show chan, show parent, show mask,
                                            show unsafeCQ, show handle,
                                            show deadline])
    call <- C.grpcChannelCreateRegisteredCall chan parent mask unsafeCQ
                                              handle deadline C.reserved
    return $ Right $ ClientCall call

-- | Create the call object to handle a registered call.
serverRequestCall :: C.Server
                  -> CompletionQueue
                  -> TimeoutSeconds
                  -> RegisteredMethod
                  -> MetadataMap
                  -> IO (Either GRPCIOError ServerCall)
serverRequestCall
  server cq@CompletionQueue{..} timeLimit RegisteredMethod{..} initMeta =
    withPermission Push cq $ do
      -- TODO: Is gRPC supposed to populate this deadline?
      -- NOTE: the below stuff is freed when we free the call we return.
      deadline <- C.secondsToDeadline timeLimit
      callPtr <- malloc
      metadataArrayPtr <- C.metadataArrayCreate
      metadataArray <- peek metadataArrayPtr
      #ifdef DEBUG
      metaCount <- C.metadataArrayGetCount metadataArray
      metaCap <- C.metadataArrayGetCapacity metadataArray
      kvPtr <- C.metadataArrayGetMetadata metadataArray
      grpcDebug $ "grpc-created meta: count: " ++ show metaCount
                  ++ " capacity: " ++ show metaCap ++ " ptr: " ++ show kvPtr
      #endif
      metadataContents <- C.createMetadata initMeta
      C.metadataArraySetMetadata metadataArray metadataContents
      bbPtr <- malloc
      tag <- newTag cq
      callError <- C.grpcServerRequestRegisteredCall
                     server methodHandle callPtr deadline
                     metadataArray bbPtr unsafeCQ unsafeCQ tag
      grpcDebug $ "serverRequestCall(R): callError: "
                   ++ show callError
      if callError /= C.CallOk
         then do grpcDebug "serverRequestCall(R): callError. cleaning up"
                 failureCleanup deadline callPtr metadataArrayPtr bbPtr
                 return $ Left $ GRPCIOCallError callError
         else do pluckResult <- pluck cq tag timeLimit
                 grpcDebug "serverRequestCall(R): finished pluck."
                 case pluckResult of
                   Left x -> do
                     grpcDebug "serverRequestCall(R): cleanup pluck err"
                     failureCleanup deadline callPtr metadataArrayPtr bbPtr
                     return $ Left x
                   Right () -> do
                     rawCall <- peek callPtr
                     let assembledCall = ServerCall rawCall metadataArrayPtr
                                           bbPtr Nothing deadline
                     return $ Right assembledCall
      -- TODO: see TODO for failureCleanup in serverRequestCall.
      where failureCleanup deadline callPtr metadataArrayPtr bbPtr = forkIO $ do
              threadDelaySecs 30
              grpcDebug "serverRequestCall(R): doing delayed cleanup."
              C.timespecDestroy deadline
              free callPtr
              C.metadataArrayDestroy metadataArrayPtr
              free bbPtr

-- | Register the server's completion queue. Must be done before the server is
-- started.
serverRegisterCompletionQueue :: C.Server -> CompletionQueue -> IO ()
serverRegisterCompletionQueue server CompletionQueue{..} =
  C.grpcServerRegisterCompletionQueue server unsafeCQ C.reserved

serverShutdownAndNotify :: C.Server -> CompletionQueue -> C.Tag -> IO ()
serverShutdownAndNotify server CompletionQueue{..} tag =
  C.grpcServerShutdownAndNotify server unsafeCQ tag
