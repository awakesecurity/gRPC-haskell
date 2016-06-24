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

import           Control.Concurrent.STM                  (atomically, check)
import           Control.Concurrent.STM.TVar             (newTVarIO, readTVar,
                                                          writeTVar)
import           Control.Exception                       (bracket)
import           Control.Monad                           (liftM2)
import           Data.IORef                              (newIORef)
import           Data.List                               (intersperse)
import           Foreign.Marshal.Alloc                   (free, malloc)
import           Foreign.Ptr                             (nullPtr)
import           Foreign.Storable                        (peek)
import qualified Network.GRPC.Unsafe                     as C
import qualified Network.GRPC.Unsafe.Constants           as C
import qualified Network.GRPC.Unsafe.Metadata            as C
import qualified Network.GRPC.Unsafe.Op                  as C
import qualified Network.GRPC.Unsafe.Time                as C
import           System.Clock                            (getTime, Clock(..))
import           System.Timeout                          (timeout)

import           Network.GRPC.LowLevel.Call
import           Network.GRPC.LowLevel.GRPC
import           Network.GRPC.LowLevel.CompletionQueue.Internal
import qualified Network.GRPC.Unsafe.ByteBuffer as C

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
  grpcDebug $ "Got CQ loop shutdown result of: " ++ show loopRes
  case loopRes of
    Nothing -> return $ Left GRPCIOShutdownFailure
    Just () -> C.grpcCompletionQueueDestroy unsafeCQ >> return (Right ())

  where drainLoop :: IO ()
        drainLoop = do
          grpcDebug "drainLoop: before next() call"
          ev <- C.withDeadlineSeconds 1 $ \deadline ->
                  C.grpcCompletionQueueNext unsafeCQ deadline C.reserved
          grpcDebug $ "drainLoop: next() call got " ++ show ev
          case (C.eventCompletionType ev) of
            C.QueueShutdown -> return ()
            C.QueueTimeout -> drainLoop
            C.OpComplete -> drainLoop

channelCreateCall :: C.Channel
                  -> (Maybe ServerCall)
                  -> C.PropagationMask
                  -> CompletionQueue
                  -> C.CallHandle
                  -> C.CTimeSpecPtr
                  -> IO (Either GRPCIOError ClientCall)
channelCreateCall
  chan parent mask cq@CompletionQueue{..} handle deadline =
  withPermission Push cq $ do
    let parentPtr = maybe (C.Call nullPtr) unServerCall parent
    grpcDebug $ "channelCreateCall: call with "
                ++ concat (intersperse " " [show chan, show parentPtr,
                                            show mask,
                                            show unsafeCQ, show handle,
                                            show deadline])
    call <- C.grpcChannelCreateRegisteredCall chan parentPtr mask unsafeCQ
                                              handle deadline C.reserved
    return $ Right $ ClientCall call

-- | Create the call object to handle a registered call.
serverRequestCall :: C.Server
                  -> CompletionQueue
                  -> RegisteredMethod
                  -> IO (Either GRPCIOError ServerCall)
serverRequestCall
  server cq@CompletionQueue{..} RegisteredMethod{..} =
    withPermission Push cq $
      bracket (liftM2 (,) malloc malloc)
              (\(p1,p2) -> free p1 >> free p2)
              $ \(deadlinePtr, callPtr) ->
        C.withByteBufferPtr $ \bbPtr ->
          C.withMetadataArrayPtr $ \metadataArrayPtr -> do
            metadataArray <- peek metadataArrayPtr
            tag <- newTag cq
            grpcDebug $ "serverRequestCall(R): tag is " ++ show tag
            callError <- C.grpcServerRequestRegisteredCall
                           server methodHandle callPtr deadlinePtr
                           metadataArray bbPtr unsafeCQ unsafeCQ tag
            grpcDebug $ "serverRequestCall(R): callError: "
                         ++ show callError
            if callError /= C.CallOk
               then do grpcDebug "serverRequestCall(R): callError. cleaning up"
                       return $ Left $ GRPCIOCallError callError
               else do pluckResult <- pluck cq tag Nothing
                       grpcDebug $ "serverRequestCall(R): finished pluck:"
                                   ++ show pluckResult
                       case pluckResult of
                         Left x -> do
                           grpcDebug "serverRequestCall(R): cleanup pluck err"
                           return $ Left x
                         Right () -> do
                           rawCall <- peek callPtr
                           deadline <- convertDeadline deadlinePtr
                           payload <- convertPayload bbPtr
                           meta <- convertMeta metadataArrayPtr
                           let assembledCall = ServerCall rawCall
                                                          meta
                                                          payload
                                                          deadline
                           grpcDebug "serverRequestCall(R): About to return"
                           return $ Right assembledCall
      where convertDeadline deadline = do
              --gRPC gives us a deadline that is just a delta, so we convert it
              --to a proper deadline.
              deadline' <- C.timeSpec <$> peek deadline
              now <- getTime Monotonic
              return $ now + deadline'
            convertPayload bbPtr = do
              -- TODO: the reason this returns @Maybe ByteString@ is because the
              -- gRPC library calls the  underlying out parameter
              -- "optional_payload". I am not sure exactly in what cases it
              -- won't be present. The C++ library checks a
              -- has_request_payload_ bool and passes in nullptr to
              -- request_registered_call if the bool is false, so we
              -- may be able to do the payload present/absent check earlier.
              bb@(C.ByteBuffer rawPtr) <- peek bbPtr
              if rawPtr == nullPtr
                 then return Nothing
                 else do bs <- C.copyByteBufferToByteString bb
                         return $ Just bs
            convertMeta requestMetadataRecv = do
              mArray <- peek requestMetadataRecv
              metamap <- C.getAllMetadataArray mArray
              return metamap

-- | Register the server's completion queue. Must be done before the server is
-- started.
serverRegisterCompletionQueue :: C.Server -> CompletionQueue -> IO ()
serverRegisterCompletionQueue server CompletionQueue{..} =
  C.grpcServerRegisterCompletionQueue server unsafeCQ C.reserved

serverShutdownAndNotify :: C.Server -> CompletionQueue -> C.Tag -> IO ()
serverShutdownAndNotify server CompletionQueue{..} tag =
  C.grpcServerShutdownAndNotify server unsafeCQ tag
