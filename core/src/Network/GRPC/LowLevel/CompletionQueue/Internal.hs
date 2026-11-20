{-# LANGUAGE RecordWildCards #-}

module Network.GRPC.LowLevel.CompletionQueue.Internal where

import Control.Concurrent.Async (Async, async, waitCatch)
import Control.Concurrent.MVar (
  MVar,
  newEmptyMVar,
  readMVar,
  tryPutMVar,
 )
import Control.Concurrent.STM (atomically, check, retry)
import Control.Concurrent.STM.TVar (
  TVar,
  modifyTVar',
  readTVar,
  swapTVar,
  writeTVar,
 )
import Control.Exception (
  SomeException,
  bracket,
  catch,
  finally,
  mask_,
  onException,
  try,
  uninterruptibleMask_,
 )
import Control.Monad
import Data.Function (fix)
import Foreign.Ptr (Ptr, nullPtr)
import Foreign.StablePtr (
  StablePtr,
  castPtrToStablePtr,
  castStablePtrToPtr,
  deRefStablePtr,
  freeStablePtr,
  newStablePtr,
 )
import Foreign.Storable (peek)
import Network.GRPC.LowLevel.GRPC
import qualified Network.GRPC.Unsafe as C
import qualified Network.GRPC.Unsafe.Time as C
import System.Timeout (timeout)

-- NOTE: each 'CompletionQueue' is read by exactly one thread, its dispatcher
-- (see 'startDispatcher'), which loops on grpc_completion_queue_next.
--
-- Every operation is tagged with a 'StablePtr' to a fresh 'MVar' (see
-- 'startCompletion'). When the operation completes, the dispatcher puts the
-- event into the 'MVar' and frees the 'StablePtr'. The thread that started the
-- operation waits for the event in the 'MVar' (see 'awaitCompletion'). Any
-- number of threads can wait at once.
--
-- To shut down (see 'shutdownCompletionQueue'):
-- 1. Set the shuttingDown 'TVar' to 'True'. When this is set, no new work can
--    be pushed onto the queue.
-- 2. Wait until no threads are pushing, as counted by 'currentPushers', then
--    call grpc_completion_queue_shutdown.
-- 3. gRPC delivers every outstanding event before next reports that the queue
--    has shut down. The dispatcher then destroys the queue, releases the gRPC
--    reference it holds (taken with grpc_init in 'createCompletionQueue'), and
--    completes 'dispatcherThread'. Holding that reference keeps gRPC alive for
--    as long as the dispatcher can call into it, even if shutdown timed out.

-- TODO: 'currentPushers' currently imposes an arbitrary limit on the number of
-- concurrent pushers to the CQ, but I don't know what the limit should be set
-- to. I haven't found any documentation that suggests there is a limit imposed
-- by the gRPC library, but there might be. Need to investigate further.

-- | Wraps the state necessary to use a gRPC completion queue. Completion queues
-- are used to wait for batches gRPC operations ('Op's) to finish running, as
-- well as wait for various other operations, such as server shutdown, pinging,
-- checking to see if we've been disconnected, and so forth.
data CompletionQueue = CompletionQueue
  { unsafeCQ :: C.CompletionQueue
  -- ^ All access to this field must be
  -- guarded by a check of 'shuttingDown'.
  , currentPushers :: TVar Int
  -- ^ Used to prevent new work from
  -- being pushed onto the queue when
  -- the queue begins to shut down.
  , shuttingDown :: TVar Bool
  -- ^ Used to prevent new work from
  -- being pushed onto the queue when
  -- the queue begins to shut down.
  , dispatcherThread :: Async ()
  -- ^ The dispatcher, which completes once the
  -- queue has been drained and destroyed.
  }

instance Show CompletionQueue where show = show . unsafeCQ

type TimeoutSeconds = Int

-- | Safely brackets an operation that pushes work onto the given
-- 'CompletionQueue'.
withPermission ::
  CompletionQueue ->
  IO (Either GRPCIOError a) ->
  IO (Either GRPCIOError a)
withPermission cq act = bracket acquire release $ \gotResource ->
  if gotResource then act else return (Left GRPCIOShutdown)
  where
    acquire = atomically $ do
      isShuttingDown <- readTVar (shuttingDown cq)
      unless isShuttingDown $ do
        currCount <- readTVar (currentPushers cq)
        if currCount < maxWorkPushers
          then writeTVar (currentPushers cq) (currCount + 1)
          else retry
      return (not isShuttingDown)
    release gotResource =
      when gotResource $
        atomically $
          modifyTVar' (currentPushers cq) (subtract 1)

type CompletionVar = MVar (Either (C.Event -> IO ()) C.Event)

-- | A tag handed to gRPC, plus the variable the dispatcher fills with its
-- event.
data Completion = Completion
  { completionTag :: C.Tag
  , completionVar :: CompletionVar
  }

readEvent :: Completion -> IO C.Event
readEvent Completion{..} =
  either (const (error "readEvent: abandoned")) id <$> readMVar completionVar

-- | Creates a fresh tag and starts an operation with it. The tag is a
-- 'StablePtr' to a fresh 'MVar', which the dispatcher fills with the
-- operation's event (and frees the 'StablePtr') once gRPC posts the tag.
--
-- If @start@ returns 'Left' or throws, gRPC never saw the tag, so no event will
-- come and the 'StablePtr' is freed here.
--
-- This runs under 'mask_'. The only interruptible point in the callers'
-- @start@ actions is the STM 'retry' in 'withPermission', which happens before
-- gRPC sees the tag; once the foreign call has accepted it, nothing
-- interruptible runs before this returns.
startCompletion ::
  (C.Tag -> IO (Either GRPCIOError ())) ->
  IO (Either GRPCIOError Completion)
startCompletion start = mask_ $ do
  var <- newEmptyMVar
  sp <- newStablePtr var
  let tag = C.Tag (castStablePtrToPtr sp)
  grpcDebug $ "startCompletion: starting operation with tag=" ++ show tag
  r <- start tag `onException` freeStablePtr sp
  case r of
    Left err -> do
      freeStablePtr sp
      return (Left err)
    Right () -> return (Right (Completion tag var))

-- | Waits for the event of an operation started with 'startCompletion'.
--
-- With @Nothing@, blocks uninterruptibly until the event arrives. Callers such
-- as 'Network.GRPC.LowLevel.Op.runOps' rely on this to keep buffers alive
-- while gRPC can still write to them. When doing client ops, the wait will end
-- when the deadline associated with the 'ClientCall' expires.
--
-- With @Just n@, waits at most @n@ seconds and returns 'GRPCIOTimeout' if the
-- event has not arrived; the same 'Completion' can then be awaited again.
awaitCompletion ::
  Completion ->
  Maybe TimeoutSeconds ->
  IO (Either GRPCIOError ())
awaitCompletion c@Completion{..} mwait = do
  grpcDebug $
    "awaitCompletion: waiting for tag="
      ++ show completionTag
      ++ ",mwait="
      ++ show mwait
  mev <- case mwait of
    Nothing -> Just <$> uninterruptibleMask_ (readEvent c)
    Just n -> timeout (n * 10 ^ (6 :: Int)) (readEvent c)
  grpcDebug $ "awaitCompletion finished: " ++ show mev
  pure (maybe (Left GRPCIOTimeout) eventResult mev)

awaitCompletionOr :: Completion -> (C.Event -> IO ()) -> IO (Either GRPCIOError ())
awaitCompletionOr c@Completion{..} abandon = do
  ev <- readEvent c `onException` do
    gaveUp <- tryPutMVar completionVar (Left abandon)
    unless gaveUp (readEvent c >>= abandon)
  pure (eventResult ev)

eventResult :: C.Event -> Either GRPCIOError ()
eventResult ev
  | isEventSuccessful ev = Right ()
  | otherwise = eventToError ev

requestCall ::
  CompletionQueue ->
  Ptr C.Call ->
  IO () ->
  (C.Tag -> IO C.CallError) ->
  IO a ->
  IO (Either GRPCIOError a)
requestCall scq call release request build = do
  started <-
    startCompletion (\tag -> withPermission scq (throwIfCallError <$> request tag))
      `onException` release
  case started of
    Left e -> release >> return (Left e)
    Right c -> do
      r <- awaitCompletionOr c (\ev -> cancelArrived ev call `finally` release)
      grpcDebug ("serverRequestCall: awaitCompletion finished: " ++ show r)
      traverse (const build) r `finally` release

abandonFailed :: SomeException -> IO ()
abandonFailed e = grpcDebug ("dispatcher: abandon threw: " ++ show e)

cancelArrived :: C.Event -> Ptr C.Call -> IO ()
cancelArrived ev call =
  when (isEventSuccessful ev) $ do
    c <- peek call
    C.grpcCallCancel c C.reserved
    C.grpcCallUnref c

-- | Translate 'C.Event' to an error. The caller is responsible for ensuring
-- that the event actually corresponds to an error condition; a successful event
-- will be translated to a 'GRPCIOUnknownError'.
eventToError :: C.Event -> (Either GRPCIOError a)
eventToError (C.Event C.QueueShutdown _ _) = Left GRPCIOShutdown
eventToError (C.Event C.QueueTimeout _ _) = Left GRPCIOTimeout
eventToError (C.Event C.OpComplete False _) = Left GRPCIOTimeout
eventToError _ = Left GRPCIOUnknownError

-- | Returns true iff the given grpc_event was a success.
isEventSuccessful :: C.Event -> Bool
isEventSuccessful (C.Event C.OpComplete True _) = True
isEventSuccessful _ = False

maxWorkPushers :: Int
maxWorkPushers = 100 -- TODO: figure out what this should be.

--------------------------------------------------------------------------------
-- Dispatcher

-- | Forks the dispatcher thread for a queue. The dispatcher takes over the
-- gRPC reference that 'createCompletionQueue' acquired for it (with
-- grpc_init). It runs until the queue has been shut down and drained, then
-- destroys the queue, releases that reference, and completes.
--
-- Because gRPC's init/shutdown calls are reference-counted, holding the
-- reference until then means gRPC cannot be torn down while the dispatcher can
-- still call into it, even if 'shutdownCompletionQueue' timed out and the user
-- has already called 'Network.GRPC.LowLevel.GRPC.stopGRPC'.
--
-- If the dispatcher throws, it is restarted; it only exits once the queue
-- reports shutdown.
startDispatcher :: C.CompletionQueue -> IO (Async ())
startDispatcher cq =
  -- Masked so that nothing can kill the dispatcher: it only ever blocks inside
  -- the foreign call to grpc_completion_queue_next.
  async . mask_ $
    fix (\again -> try (dispatcher cq) >>= either (restart again) return)
      >> C.grpcCompletionQueueDestroy cq
      >> C.grpcShutdown
  where
    restart :: IO () -> SomeException -> IO ()
    restart again _ = again

-- | Reads events from the queue and delivers each one to the 'MVar' its tag
-- points to, until the queue reports that it has shut down. Must be the only
-- reader of the queue.
dispatcher :: C.CompletionQueue -> IO ()
dispatcher cq = C.withInfiniteDeadline loop
  where
    loop deadline = do
      ev <- C.grpcCompletionQueueNext cq deadline C.reserved
      case C.eventCompletionType ev of
        C.QueueShutdown -> return ()
        C.QueueTimeout -> loop deadline
        C.OpComplete -> do
          let C.Tag p = C.eventTag ev
          unless (p == nullPtr) $ do
            let sp = castPtrToStablePtr p :: StablePtr CompletionVar
            var <- deRefStablePtr sp
            delivered <- tryPutMVar var (Right ev)
            freeStablePtr sp
            unless delivered $
              readMVar var >>= either (\abandon -> abandon ev `catch` abandonFailed) (const (return ()))
          grpcDebug $ "dispatcher: delivered " ++ show ev
          loop deadline

-- | Shuts down the completion queue. No new work can be pushed onto the queue
-- after this is called. The dispatcher destroys the queue once every operation
-- already in flight has completed. Errors with 'GRPCIOShutdownFailure' if that
-- doesn't happen within 5 seconds; in that case the queue is still destroyed
-- if and when the outstanding operations do complete. Until then the
-- dispatcher keeps its gRPC reference, so gRPC stays initialized even if
-- 'Network.GRPC.LowLevel.GRPC.stopGRPC' is called in the meantime.
shutdownCompletionQueue :: CompletionQueue -> IO (Either GRPCIOError ())
shutdownCompletionQueue CompletionQueue{..} = do
  uninterruptibleMask_ $ do
    alreadyShuttingDown <- atomically $ swapTVar shuttingDown True
    unless alreadyShuttingDown $ do
      atomically $ readTVar currentPushers >>= check . (== 0)
      C.grpcCompletionQueueShutdown unsafeCQ
  res <- timeout (5 * 10 ^ (6 :: Int)) (waitCatch dispatcherThread)
  grpcDebug $ "Got CQ dispatcher shutdown result of: " ++ show res
  return $ case res of
    Nothing -> Left GRPCIOShutdownFailure
    Just _ -> Right ()
