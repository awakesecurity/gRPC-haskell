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

{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE MultiWayIf          #-}
{-# LANGUAGE PolyKinds           #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}

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

import           Control.Concurrent.STM                         (atomically,
                                                                 check)
import           Control.Concurrent.STM.TVar                    (newTVarIO,
                                                                 readTVar,
                                                                 writeTVar)
import           Control.Exception                              (bracket)
import           Control.Monad                                  (liftM2)
import           Control.Monad.Managed
import           Control.Monad.Trans.Class                      (MonadTrans (lift))
import           Control.Monad.Trans.Except
import           Data.IORef                                     (newIORef)
import           Data.List                                      (intersperse)
import           Foreign.Marshal.Alloc                          (free, malloc)
import           Foreign.Ptr                                    (Ptr, nullPtr)
import           Foreign.Storable                               (Storable, peek)
import qualified Network.GRPC.Unsafe                            as C
import qualified Network.GRPC.Unsafe.Constants                  as C
import qualified Network.GRPC.Unsafe.Metadata                   as C
import qualified Network.GRPC.Unsafe.Op                         as C
import qualified Network.GRPC.Unsafe.Time                       as C
import           System.Clock                                   (Clock (..),
                                                                 getTime)
import           System.Timeout                                 (timeout)

import           Network.GRPC.LowLevel.Call
import           Network.GRPC.LowLevel.CompletionQueue.Internal
import           Network.GRPC.LowLevel.GRPC
import qualified Network.GRPC.Unsafe.ByteBuffer                 as C

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
  return CompletionQueue{..}

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
shutdownCompletionQueue CompletionQueue{..} = do
  atomically $ writeTVar shuttingDown True
  atomically $ do
    readTVar currentPushers  >>= check . (==0)
    readTVar currentPluckers >>= check . (==0)
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
          case C.eventCompletionType ev of
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
                  -> RegisteredMethod mt
                  -> IO (Either GRPCIOError ServerCall)
serverRequestCall s cq@CompletionQueue{.. } rm =
  -- NB: The method type dictates whether or not a payload is present, according
  -- to the payloadHandling function. We do not allocate a buffer for the
  -- payload when it is not present.
  withPermission Push cq . with allocs $ \(dead, call, pay, meta) -> do
    dbug "pre-pluck block"
    withPermission Pluck cq $ do
      md  <- peek meta
      tag <- newTag cq
      dbug $ "got pluck permission, registering call for tag=" ++ show tag
      ce <- C.grpcServerRequestRegisteredCall s (methodHandle rm) call dead md pay unsafeCQ unsafeCQ tag
      runExceptT $ case ce of
        C.CallOk -> do
          ExceptT $ do
            r <- pluck' cq tag Nothing
            dbug $ "pluck' finished:" ++ show r
            return r
          lift $
            ServerCall
            <$> peek call
            <*> C.getAllMetadataArray md
            <*> (if havePay then toBS pay else return Nothing)
            <*> liftM2 (+) (getTime Monotonic) (C.timeSpec <$> peek dead)
                -- gRPC gives us a deadline that is just a delta, so we convert
                -- it to a proper deadline.
        _ -> do
          lift $ dbug $ "Throwing callError: " ++ show ce
          throwE (GRPCIOCallError ce)
  where
    allocs = (,,,) <$> ptr <*> ptr <*> pay <*> md
      where
        md  = managed C.withMetadataArrayPtr
        pay = if havePay then managed C.withByteBufferPtr else return nullPtr
        ptr :: forall a. Storable a => Managed (Ptr a)
        ptr = managed (bracket malloc free)
    dbug    = grpcDebug . ("serverRequestCall(R): " ++)
    havePay = payloadHandling (methodType rm) /= C.SrmPayloadNone
    toBS p  = peek p >>= \bb@(C.ByteBuffer rawPtr) ->
      if | rawPtr == nullPtr -> return Nothing
         | otherwise         -> Just <$> C.copyByteBufferToByteString bb

-- | Register the server's completion queue. Must be done before the server is
-- started.
serverRegisterCompletionQueue :: C.Server -> CompletionQueue -> IO ()
serverRegisterCompletionQueue server CompletionQueue{..} =
  C.grpcServerRegisterCompletionQueue server unsafeCQ C.reserved

serverShutdownAndNotify :: C.Server -> CompletionQueue -> C.Tag -> IO ()
serverShutdownAndNotify server CompletionQueue{..} tag =
  C.grpcServerShutdownAndNotify server unsafeCQ tag
