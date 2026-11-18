{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ViewPatterns #-}

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
module Network.GRPC.LowLevel.CompletionQueue (
  CompletionQueue,
  withCompletionQueue,
  createCompletionQueue,
  shutdownCompletionQueue,
  startBatch,
  channelCreateCall,
  TimeoutSeconds,
  serverRegisterCompletionQueue,
  serverShutdownAndNotify,
  serverRequestCall,
  Operation (..),
  runCompletion,
)
where

import Control.Concurrent.MVar (MVar, newEmptyMVar, putMVar)
import Control.Concurrent.STM.TVar (newTVarIO)
import Control.Exception (bracket, mask_)
import Control.Monad (unless)
import Data.List (intersperse)
import Foreign.Marshal.Alloc (free, malloc)
import Foreign.Ptr (Ptr, nullPtr)
import Foreign.Storable (peek)
import Network.GRPC.LowLevel.Call
import Network.GRPC.LowLevel.CompletionQueue.Internal
import Network.GRPC.LowLevel.GRPC
import qualified Network.GRPC.Unsafe as C
import qualified Network.GRPC.Unsafe.ByteBuffer as C
import qualified Network.GRPC.Unsafe.Constants as C
import qualified Network.GRPC.Unsafe.Metadata as C
import qualified Network.GRPC.Unsafe.Op as C
import qualified Network.GRPC.Unsafe.Time as C

withCompletionQueue :: GRPC -> (CompletionQueue -> IO a) -> IO a
withCompletionQueue grpc =
  bracket
    (createCompletionQueue grpc)
    shutdownCompletionQueue

-- | Creates a completion queue and starts its dispatcher thread. The queue
-- must eventually be shut down with 'shutdownCompletionQueue'.
createCompletionQueue :: GRPC -> IO CompletionQueue
createCompletionQueue _ = mask_ $ do
  unsafeCQ <- C.grpcCompletionQueueCreateForNext C.reserved
  currentPushers <- newTVarIO 0
  shuttingDown <- newTVarIO False
  dispatcherDone <- newEmptyMVar
  startDispatcher unsafeCQ dispatcherDone
  return CompletionQueue{..}

-- | Very simple wrapper around 'grpcCallStartBatch'. Throws 'GRPCIOShutdown'
-- without calling 'grpcCallStartBatch' if the queue is shutting down.
-- Throws 'CallError' if 'grpcCallStartBatch' returns a non-OK code.
startBatch ::
  CompletionQueue ->
  C.Call ->
  C.OpArray ->
  Int ->
  C.Tag ->
  IO (Either GRPCIOError ())
startBatch cq call opArray opArraySize tag =
  withPushPermission cq $ fmap throwIfCallError $ do
    grpcDebug $
      "startBatch: calling grpc_call_start_batch with pointers: "
        ++ show call
        ++ " "
        ++ show opArray
    res <- C.grpcCallStartBatch call opArray opArraySize tag C.reserved
    grpcDebug "startBatch: grpc_call_start_batch call returned."
    return res

channelCreateCall ::
  C.Channel ->
  Maybe (ServerCall a) ->
  C.PropagationMask ->
  CompletionQueue ->
  C.CallHandle ->
  C.CTimeSpecPtr ->
  IO (Either GRPCIOError ClientCall)
channelCreateCall
  chan
  parent
  mask
  cq@CompletionQueue{..}
  handle
  deadline =
    withPushPermission cq $ do
      let parentPtr = maybe (C.Call nullPtr) unsafeSC parent
      grpcDebug $
        "channelCreateCall: call with "
          ++ concat
            ( intersperse
                " "
                [ show chan
                , show parentPtr
                , show mask
                , show unsafeCQ
                , show handle
                , show deadline
                ]
            )
      call <-
        C.grpcChannelCreateRegisteredCall
          chan
          parentPtr
          mask
          unsafeCQ
          handle
          deadline
          C.reserved
      return $ Right $ ClientCall call

-- | Create the call object to handle a registered call. Blocks until a call
-- arrives or the server shuts down, in which case the result is
-- 'GRPCIOShutdown'.
--
-- If the calling thread is interrupted while waiting, the request stays
-- registered with gRPC (it cannot be withdrawn); the call that eventually
-- satisfies it is cancelled.
serverRequestCall ::
  RegisteredMethod mt ->
  C.Server ->
  -- | server CQ
  CompletionQueue ->
  -- | call CQ
  CompletionQueue ->
  IO (Either GRPCIOError (ServerCall (MethodPayload mt)))
serverRequestCall rm s scq ccq = runCompletion $ do
  -- NB: These are owned by the completion callback, not by this thread: gRPC
  -- writes to them when a call arrives, however long that takes.
  dead <- malloc :: IO (Ptr C.CTimeSpec)
  call <- malloc :: IO (Ptr C.Call)
  -- NB: The method type dictates whether or not a payload is present, according
  -- to the payloadHandling function. We do not allocate a buffer for the
  -- payload when it is not present.
  pay <-
    if payloadHandling (methodType rm) == C.SrmPayloadNone
      then return nullPtr
      else C.createReceivingByteBuffer
  meta <- C.metadataArrayCreate
  let release = do
        free dead
        free call
        unless (pay == nullPtr) $ C.destroyReceivingByteBuffer pay
        C.metadataArrayDestroy meta
      build =
        ServerCall
          <$> peek call
          <*> return ccq
          <*> (peek meta >>= C.getAllMetadataArray)
          <*> extractPayload rm pay
          <*> (C.timeSpec <$> peek dead)
      request tag = do
        md <- peek meta
        dbug $ "registering call for tag=" ++ show tag
        ce <-
          C.grpcServerRequestRegisteredCall
            s
            (methodHandle rm)
            call
            dead
            md
            pay
            (unsafeCQ ccq)
            (unsafeCQ scq)
            tag
        dbug $ "request returned: " ++ show ce
        return ce
  return (serverCallOperation scq unsafeSC release build request)
  where
    dbug = grpcDebug . ("serverRequestCall(R): " ++)

-- | Register the server's completion queue. Must be done before the server is
-- started.
serverRegisterCompletionQueue :: C.Server -> CompletionQueue -> IO ()
serverRegisterCompletionQueue server CompletionQueue{..} =
  C.grpcServerRegisterCompletionQueue server unsafeCQ C.reserved

-- | Begins shutting down the server. The returned 'MVar' is filled once the
-- server has no more calls in progress. Returns 'GRPCIOShutdown' if the given
-- queue is already shutting down.
serverShutdownAndNotify ::
  C.Server ->
  CompletionQueue ->
  IO (Either GRPCIOError (MVar ()))
serverShutdownAndNotify server cq = mask_ . withPushPermission cq $ do
  done <- newEmptyMVar
  tag <- newCompletionTag (\_ -> putMVar done ())
  C.grpcServerShutdownAndNotify server (unsafeCQ cq) tag
  return (Right done)
