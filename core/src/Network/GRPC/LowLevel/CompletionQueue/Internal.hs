{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Network.GRPC.LowLevel.CompletionQueue.Internal where

import Control.Concurrent (forkIO)
import Control.Concurrent.MVar (
  MVar,
  newEmptyMVar,
  putMVar,
  readMVar,
  takeMVar,
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
  finally,
  mask_,
  onException,
  throwIO,
  try,
 )
import Control.Monad
import Data.Foldable (for_)
import Foreign.StablePtr (
  StablePtr,
  castPtrToStablePtr,
  castStablePtrToPtr,
  deRefStablePtr,
  freeStablePtr,
  newStablePtr,
 )
import Network.GRPC.LowLevel.GRPC
import qualified Network.GRPC.Unsafe as C
import qualified Network.GRPC.Unsafe.Time as C
import System.Timeout (timeout)

-- NOTE: gRPC offers two ways to read a completion queue: next and pluck. Pluck
-- lets each waiting thread ask for its own tag, but gRPC allows at most
-- 'C.maxCompletionQueuePluckers' concurrent pluckers per queue, so a handful
-- of threads blocked on long-running operations (e.g. receiving on an idle
-- stream) would starve every other operation on that queue.
--
-- Instead, every 'CompletionQueue' is a "next" queue with a single dispatcher
-- thread. The dispatcher is the only thread that ever reads the queue. Each
-- tag we hand to gRPC is a 'StablePtr' to a completion callback; when the
-- event for a tag arrives, the dispatcher runs the callback, which typically
-- copies results out of C buffers, frees those buffers, and fills an 'MVar'
-- that the thread which started the operation is waiting on. See
-- 'Operation' and 'runCompletion'.
--
-- Two rules follow from this design:
--
-- 1. Completion callbacks run on the dispatcher thread, so they must be short
--    and must never block.
-- 2. Memory handed to gRPC for an operation is owned by that operation's
--    completion callback, not by the thread that started it. gRPC may write to
--    that memory until the completion event is delivered, regardless of what
--    happens to the waiting thread in the meantime.

-- NOTE: There is one more possible race condition: pushing work onto the queue
-- after we begin to shut down.
-- Solution: a counter of threads currently starting work, which must reach
-- zero before the shutdown can start.

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
  -- ^ Work may only be pushed onto this queue
  -- under 'withPushPermission'. Only the
  -- dispatcher thread may read from it.
  , currentPushers :: TVar Int
  -- ^ Used to prevent new work from
  -- being pushed onto the queue when
  -- the queue begins to shut down.
  , shuttingDown :: TVar Bool
  -- ^ Used to prevent new work from being
  -- started on the queue when the queue
  -- begins to shut down.
  , dispatcherDone :: MVar ()
  -- ^ Filled once the dispatcher thread has
  -- seen the queue shut down and destroyed
  -- it.
  }

instance Show CompletionQueue where show = show . unsafeCQ

type TimeoutSeconds = Int

--------------------------------------------------------------------------------
-- Completion tags

-- | The action to run when the event for a tag is delivered. The 'Bool' is the
-- event's success flag.
newtype Completion = Completion (Bool -> IO ())

-- | Create a 'C.Tag' which, when its event is read from a 'CompletionQueue',
-- causes the dispatcher to run the given callback. Every tag must be consumed
-- exactly once: either by the dispatcher (when gRPC delivers its event) or by
-- 'dropCompletionTag' (when gRPC refused to start the operation, so no event
-- will ever be delivered).
newCompletionTag :: (Bool -> IO ()) -> IO C.Tag
newCompletionTag k = C.Tag . castStablePtrToPtr <$> newStablePtr (Completion k)

-- | Runs and releases a tag created by 'newCompletionTag'. Only the dispatcher
-- should call this.
runCompletionTag :: C.Tag -> Bool -> IO ()
runCompletionTag (C.Tag p) ok = do
  let sp = castPtrToStablePtr p :: StablePtr Completion
  Completion k <- deRefStablePtr sp
  freeStablePtr sp
  k ok

-- | Releases a tag created by 'newCompletionTag' without running it. Only
-- valid if the tag was never successfully handed to gRPC.
dropCompletionTag :: C.Tag -> IO ()
dropCompletionTag (C.Tag p) =
  freeStablePtr (castPtrToStablePtr p :: StablePtr Completion)

-- | An asynchronous gRPC operation, as run by 'runCompletion'.
data Operation a = Operation
  { opStart :: C.Tag -> IO (Either GRPCIOError ())
  -- ^ Hands the tag to gRPC. Must return 'Left' iff gRPC did not accept the
  -- operation (in which case no event will be delivered). It is responsible
  -- for obtaining permission to push work onto the queue; see
  -- 'withPushPermission'.
  , opFinish :: Bool -> IO (Either GRPCIOError a)
  -- ^ Runs on the dispatcher thread when the operation completes, before
  -- 'opRelease', and receives the event's success flag. It should copy
  -- whatever is needed out of the C resources, and must not block.
  , opRelease :: IO ()
  -- ^ Frees the C resources handed to gRPC for the operation. Run exactly
  -- once: by the dispatcher after the operation completes, or immediately if
  -- the operation could not be started.
  , opDiscard :: Maybe (a -> IO ())
  -- ^ Disposes of a successful result nobody is waiting for any more.
  -- 'Nothing' if such a result needs no disposal.
  }

-- | Runs an asynchronous gRPC operation to completion on a 'CompletionQueue'.
-- The given action allocates the operation's C resources and describes the
-- operation; it is run with asynchronous exceptions masked.
--
-- The calling thread waits interruptibly. If it receives an asynchronous
-- exception while waiting, the operation itself is not cancelled (gRPC offers
-- no way to withdraw a started operation): it stays in flight, its resources
-- are released when it completes, and its result is passed to 'opDiscard'.
--
-- An exception thrown by 'opFinish' or 'opRelease' is rethrown in the calling
-- thread.
runCompletion :: forall a. IO (Operation a) -> IO (Either GRPCIOError a)
runCompletion acquire = mask_ $ do
  Operation{..} <- acquire
  done <- newEmptyMVar :: IO (MVar (Either SomeException (Either GRPCIOError a)))
  let complete ok = do
        -- NB: 'done' must be filled whatever happens, or the waiter hangs.
        r <- try (opFinish ok `finally` opRelease)
        putMVar done r
      abandon tag = dropCompletionTag tag >> opRelease
      -- Waits for a result that the caller has given up on.
      reap = for_ opDiscard $ \discard ->
        forkIO $
          takeMVar done >>= \case
            Right (Right x) -> discard x
            _ -> return ()
  tag <- newCompletionTag complete
  -- 'opStart' can only be interrupted before it hands the tag to gRPC (while
  -- waiting for permission to push), so it is safe to clean up here.
  started <- opStart tag `onException` abandon tag
  case started of
    Left e -> do
      abandon tag
      return (Left e)
    Right () -> do
      r <- takeMVar done `onException` reap
      either throwIO return r

-- | The 'Operation' of waiting for a new call on a server. @release@ frees the
-- buffers gRPC fills in for the call, @build@ reads the call out of them, and
-- @request@ is the gRPC function that registers the request.
--
-- The operation fails with 'GRPCIOShutdown' when the server shuts down. If the
-- waiter is interrupted, the request stays registered with gRPC (it cannot be
-- withdrawn), and the call that eventually satisfies it is cancelled.
serverCallOperation ::
  -- | The server's notification queue.
  CompletionQueue ->
  -- | The underlying call, for cancelling an unwanted one.
  (sc -> C.Call) ->
  -- | release
  IO () ->
  -- | build
  IO sc ->
  -- | request
  (C.Tag -> IO C.CallError) ->
  Operation sc
serverCallOperation scq unsafeCall release build request =
  Operation
    { opStart = \tag -> withPushPermission scq (throwIfCallError <$> request tag)
    , opFinish = \ok ->
        if ok then Right <$> build else return (Left GRPCIOShutdown)
    , opRelease = release
    , opDiscard = Just $ \sc -> do
        C.grpcCallCancel (unsafeCall sc) C.reserved
        C.grpcCallUnref (unsafeCall sc)
    }

--------------------------------------------------------------------------------
-- Permission to push work

-- | Safely brackets an operation that pushes work onto the given
-- 'CompletionQueue'. Returns 'GRPCIOShutdown' without running the operation if
-- the queue is shutting down.
withPushPermission ::
  CompletionQueue ->
  IO (Either GRPCIOError a) ->
  IO (Either GRPCIOError a)
withPushPermission cq act = bracket acquire release $ \gotResource ->
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

maxWorkPushers :: Int
maxWorkPushers = 100 -- TODO: figure out what this should be.

--------------------------------------------------------------------------------
-- Dispatcher

-- | Forks the dispatcher thread for a queue. The dispatcher runs until the
-- queue has been shut down and drained, then destroys the queue and fills the
-- given 'MVar'.
startDispatcher :: C.CompletionQueue -> MVar () -> IO ()
startDispatcher cq done =
  -- Nothing can kill the dispatcher, because its 'ThreadId' is never exposed.
  void . forkIO $
    (dispatcher cq >> C.grpcCompletionQueueDestroy cq)
      `finally` putMVar done ()

-- | Reads events from the queue and runs their completion callbacks until the
-- queue reports that it has shut down. Must be the only reader of the queue.
dispatcher :: C.CompletionQueue -> IO ()
dispatcher cq = C.withInfiniteDeadline loop
  where
    loop deadline = do
      ev <- C.grpcCompletionQueueNext cq deadline C.reserved
      grpcDebug $ "dispatcher: next() returned " ++ show ev
      case C.eventCompletionType ev of
        C.QueueShutdown -> return ()
        C.QueueTimeout -> loop deadline
        C.OpComplete -> do
          r <- try (runCompletionTag (C.eventTag ev) (C.eventSuccess ev))
          case r of
            Left (e :: SomeException) ->
              grpcDebug $ "dispatcher: completion callback threw: " ++ show e
            Right () -> return ()
          loop deadline

-- | Shuts down the completion queue. No new work can be started on the queue
-- after this is called. The dispatcher destroys the queue once every operation
-- already in flight has completed. Errors with 'GRPCIOShutdownFailure' if that
-- doesn't happen within 5 seconds; in that case the queue is still destroyed
-- if and when the outstanding operations do complete.
shutdownCompletionQueue :: CompletionQueue -> IO (Either GRPCIOError ())
shutdownCompletionQueue CompletionQueue{..} = do
  alreadyShuttingDown <- atomically $ swapTVar shuttingDown True
  unless alreadyShuttingDown $ do
    atomically $ readTVar currentPushers >>= check . (== 0)
    C.grpcCompletionQueueShutdown unsafeCQ
  loopRes <- timeout (5 * 10 ^ (6 :: Int)) (readMVar dispatcherDone)
  grpcDebug $ "Got CQ shutdown result of: " ++ show loopRes
  return $ maybe (Left GRPCIOShutdownFailure) Right loopRes
