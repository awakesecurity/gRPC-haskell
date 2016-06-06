-- | Unlike most of the other internal low-level modules, we don't export
-- everything here. There are several things in here that, if accessed, could
-- cause race conditions, so we only expose functions that are thread safe.
-- However, some of the functions we export here can cause memory leaks if used
-- improperly.

{-# LANGUAGE RecordWildCards #-}

module Network.GRPC.LowLevel.CompletionQueue
  ( CompletionQueue
  , withCompletionQueue
  , createCompletionQueue
  , shutdownCompletionQueue
  , pluck
  , startBatch
  , channelCreateRegisteredCall
  , channelCreateCall
  , TimeoutSeconds
  , isEventSuccessful
  , serverRegisterCompletionQueue
  , serverShutdownAndNotify
  , serverRequestRegisteredCall
  , serverRequestCall
  , newTag
  )
where

import           Control.Concurrent            (forkIO, threadDelay)
import           Control.Concurrent.STM        (atomically, check, retry)
import           Control.Concurrent.STM.TVar   (TVar, modifyTVar', newTVarIO,
                                                readTVar, writeTVar)
import           Control.Exception             (bracket)
import           Data.IORef                    (IORef, atomicModifyIORef',
                                                newIORef)
import           Data.List                     (intersperse)
import           Foreign.Marshal.Alloc         (free, malloc)
import           Foreign.Ptr                   (nullPtr, plusPtr)
import           Foreign.Storable              (peek)
import qualified Network.GRPC.Unsafe           as C
import qualified Network.GRPC.Unsafe.Constants as C
import qualified Network.GRPC.Unsafe.Metadata  as C
import qualified Network.GRPC.Unsafe.Op        as C
import qualified Network.GRPC.Unsafe.Time      as C
import           System.Timeout                (timeout)

import           Network.GRPC.LowLevel.Call
import           Network.GRPC.LowLevel.GRPC

-- NOTE: the concurrency requirements for a CompletionQueue are a little
-- complicated. There are two read operations: next and pluck. We can either
-- call next on a CQ or call pluck up to 'maxCompletionQueuePluckers' times
-- concurrently, but we can't mix next and pluck calls. Fortunately, we only
-- need to use next when we are shutting down the queue. Thus, we do two things
-- to shut down:
-- 1. Set the shuttingDown 'TVar' to 'True'. When this is set, no new pluck
--    calls will be allowed to start.
-- 2. Wait until no threads are plucking, as counted by 'currentPluckers'.
-- This logic can be seen in 'pluck' and 'shutdownCompletionQueue'.

-- NOTE: There is one more possible race condition: pushing work onto the queue
-- after we begin to shut down.
-- Solution: another counter, which must reach zero before the shutdown
-- can start.

-- TODO: 'currentPushers' currently imposes an arbitrary limit on the number of
-- concurrent pushers to the CQ, but I don't know what the limit should be set
-- to. I haven't found any documentation that suggests there is a limit imposed
-- by the gRPC library, but there might be. Need to investigate further.

-- | Wraps the state necessary to use a gRPC completion queue. Completion queues
-- are used to wait for batches gRPC operations ('Op's) to finish running, as
-- well as wait for various other operations, such as server shutdown, pinging,
-- checking to see if we've been disconnected, and so forth.
data CompletionQueue = CompletionQueue {unsafeCQ        :: C.CompletionQueue,
                                        -- ^ All access to this field must be
                                        -- guarded by a check of 'shuttingDown'.
                                        currentPluckers :: TVar Int,
                                        -- ^ Used to limit the number of
                                        -- concurrent calls to pluck on this
                                        -- queue.
                                        -- The max value is set by gRPC in
                                        -- 'C.maxCompletionQueuePluckers'
                                        currentPushers  :: TVar Int,
                                        -- ^ Used to prevent new work from
                                        -- being pushed onto the queue when
                                        -- the queue begins to shut down.
                                        shuttingDown    :: TVar Bool,
                                        -- ^ Used to prevent new pluck calls on
                                        -- the queue when the queue begins to
                                        -- shut down.
                                        nextTag         :: IORef Int
                                        -- ^ Used to supply unique tags for work
                                        -- items pushed onto the queue.
                                       }

-- | Create a new 'C.Tag' for identifying work items on the 'CompletionQueue'.
-- This will eventually wrap around after reaching @maxBound :: Int@, but from a
-- practical perspective, that should be safe.
newTag :: CompletionQueue -> IO C.Tag
newTag CompletionQueue{..} = do
  i <- atomicModifyIORef' nextTag (\i -> (i+1,i))
  return $ C.Tag $ plusPtr nullPtr i

maxWorkPushers :: Int
maxWorkPushers = 100 --TODO: figure out what this should be.

data CQOpType = Push | Pluck deriving (Show, Eq, Enum)

getCount :: CQOpType -> CompletionQueue -> TVar Int
getCount Push = currentPushers
getCount Pluck = currentPluckers

getLimit :: CQOpType -> Int
getLimit Push = maxWorkPushers
getLimit Pluck = C.maxCompletionQueuePluckers

-- | Safely brackets an operation that pushes work onto or plucks results from
-- the given 'CompletionQueue'.
withPermission :: CQOpType
                  -> CompletionQueue
                  -> IO (Either GRPCIOError a)
                  -> IO (Either GRPCIOError a)
withPermission op cq f =
  bracket acquire release doOp
  where acquire = atomically $ do
          isShuttingDown <- readTVar (shuttingDown cq)
          if isShuttingDown
             then return False
             else do currCount <- readTVar $ getCount op cq
                     if currCount < getLimit op
                        then modifyTVar' (getCount op cq) (+1) >> return True
                        else retry
        doOp gotResource = if gotResource
                              then f
                              else return $ Left GRPCIOShutdown
        release gotResource =
          if gotResource
             then atomically $ modifyTVar' (getCount op cq) (subtract 1)
             else return ()

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

type TimeoutSeconds = Int

-- | Translate 'C.Event' to an error. The caller is responsible for ensuring
-- that the event actually corresponds to an error condition; a successful event
-- will be translated to a 'GRPCIOUnknownError'.
eventToError :: C.Event -> (Either GRPCIOError a)
eventToError (C.Event C.QueueShutdown _ _) = Left GRPCIOShutdown
eventToError (C.Event C.QueueTimeout _ _) = Left GRPCIOTimeout
eventToError _ = Left GRPCIOUnknownError

-- | Returns true iff the given grpc_event was a success.
isEventSuccessful :: C.Event -> Bool
isEventSuccessful (C.Event C.OpComplete True _) = True
isEventSuccessful _ = False

-- | Waits for the given number of seconds for the given tag to appear on the
-- completion queue. Throws 'GRPCIOShutdown' if the completion queue is shutting
-- down and cannot handle new requests.
pluck :: CompletionQueue -> C.Tag -> TimeoutSeconds
         -> IO (Either GRPCIOError ())
pluck cq@CompletionQueue{..} tag waitSeconds = do
  grpcDebug $ "pluck: called with tag: " ++ show tag
              ++ " and wait: " ++ show waitSeconds
  withPermission Pluck cq $ do
    C.withDeadlineSeconds waitSeconds $ \deadline -> do
      ev <- C.grpcCompletionQueuePluck unsafeCQ tag deadline C.reserved
      grpcDebug $ "pluck: finished. Event: " ++ show ev
      return $ if isEventSuccessful ev then Right () else eventToError ev

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

channelCreateRegisteredCall :: C.Channel -> C.Call -> C.PropagationMask
                               -> CompletionQueue -> C.CallHandle
                               -> C.CTimeSpecPtr
                               -> IO (Either GRPCIOError ClientCall)
channelCreateRegisteredCall
  chan parent mask cq@CompletionQueue{..} handle deadline =
  withPermission Push cq $ do
    grpcDebug $ "channelCreateRegisteredCall: call with "
                ++ concat (intersperse " " [show chan, show parent, show mask,
                                            show unsafeCQ, show handle,
                                            show deadline])
    call <- C.grpcChannelCreateRegisteredCall chan parent mask unsafeCQ
                                              handle deadline C.reserved
    return $ Right $ ClientCall call

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

-- | Create the call object to handle a registered call.
serverRequestRegisteredCall :: C.Server -> CompletionQueue -> TimeoutSeconds
                               -> RegisteredMethod -> MetadataMap
                               -> IO (Either GRPCIOError ServerRegCall)
serverRequestRegisteredCall
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
      grpcDebug $ "serverRequestRegisteredCall: callError: "
                   ++ show callError
      if callError /= C.CallOk
         then do grpcDebug "serverRequestRegisteredCall: callError. cleaning up"
                 failureCleanup deadline callPtr metadataArrayPtr bbPtr
                 return $ Left $ GRPCIOCallError callError
         else do pluckResult <- pluck cq tag timeLimit
                 grpcDebug "serverRequestRegisteredCall: finished pluck."
                 case pluckResult of
                   Left x -> do
                     grpcDebug "serverRequestRegisteredCall: cleanup pluck err"
                     failureCleanup deadline callPtr metadataArrayPtr bbPtr
                     return $ Left x
                   Right () -> do
                     rawCall <- peek callPtr
                     let assembledCall = ServerRegCall rawCall metadataArrayPtr
                                                       bbPtr Nothing deadline
                     return $ Right assembledCall
      -- TODO: see TODO for failureCleanup in serverRequestCall.
      where failureCleanup deadline callPtr metadataArrayPtr bbPtr = forkIO $ do
              threadDelaySecs 30
              grpcDebug "serverRequestRegisteredCall: doing delayed cleanup."
              C.timespecDestroy deadline
              free callPtr
              C.metadataArrayDestroy metadataArrayPtr
              free bbPtr

serverRequestCall :: C.Server -> CompletionQueue -> TimeoutSeconds
                     -> IO (Either GRPCIOError ServerUnregCall)
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
                   let call = ServerUnregCall rawCall
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

-- | Register the server's completion queue. Must be done before the server is
-- started.
serverRegisterCompletionQueue :: C.Server -> CompletionQueue -> IO ()
serverRegisterCompletionQueue server CompletionQueue{..} =
  C.grpcServerRegisterCompletionQueue server unsafeCQ C.reserved

serverShutdownAndNotify :: C.Server -> CompletionQueue -> C.Tag -> IO ()
serverShutdownAndNotify server CompletionQueue{..} tag =
  C.grpcServerShutdownAndNotify server unsafeCQ tag

threadDelaySecs :: Int -> IO ()
threadDelaySecs = threadDelay . (* 10^(6::Int))
