{-# LANGUAGE RecordWildCards #-}

module Network.GRPC.LowLevel.Op where

import           Control.Exception
import qualified Data.ByteString as B
import qualified Data.Map.Strict as M
import           Data.Maybe (catMaybes)
import           Foreign.C.Types (CInt)
import           Foreign.Marshal.Alloc (malloc, free)
import           Foreign.Ptr (Ptr)
import           Foreign.Storable (peek)
import qualified Network.GRPC.Unsafe as C
import qualified Network.GRPC.Unsafe.Metadata as C
import qualified Network.GRPC.Unsafe.ByteBuffer as C
import qualified Network.GRPC.Unsafe.Op as C

import Network.GRPC.LowLevel.GRPC
import Network.GRPC.LowLevel.CompletionQueue
import Network.GRPC.LowLevel.Call

type MetadataMap = M.Map B.ByteString B.ByteString

-- | Sum describing all possible send and receive operations that can be batched
-- and executed by gRPC. Usually these are processed in a handful of
-- combinations depending on the 'MethodType' of the call being run.
data Op = OpSendInitialMetadata MetadataMap
          | OpSendMessage B.ByteString
          | OpSendCloseFromClient
          | OpSendStatusFromServer MetadataMap C.StatusCode --TODO: Issue #6
          | OpRecvInitialMetadata
          | OpRecvMessage
          | OpRecvStatusOnClient
          | OpRecvCloseOnServer
          deriving (Eq, Show)

-- | Container holding the pointers to the C and gRPC data needed to execute the
-- corresponding 'Op'. These are obviously unsafe, and should only be used with
-- 'withOpContexts'.
data OpContext =
  OpSendInitialMetadataContext C.MetadataKeyValPtr Int
  | OpSendMessageContext C.ByteBuffer
  | OpSendCloseFromClientContext
  | OpSendStatusFromServerContext C.MetadataKeyValPtr Int C.StatusCode
  | OpRecvInitialMetadataContext (Ptr C.MetadataArray)
  | OpRecvMessageContext (Ptr C.ByteBuffer)
  | OpRecvStatusOnClientContext (Ptr C.MetadataArray) (Ptr C.StatusCode)
  | OpRecvCloseOnServerContext (Ptr CInt)

-- | Allocates and initializes the 'Opcontext' corresponding to the given 'Op'.
createOpContext :: Op -> IO OpContext
createOpContext (OpSendInitialMetadata m) =
  OpSendInitialMetadataContext
  <$> C.createMetadata m
  <*> return (M.size m)
createOpContext (OpSendMessage bs) =
  fmap OpSendMessageContext (C.createByteBuffer bs)
createOpContext (OpSendCloseFromClient) = return OpSendCloseFromClientContext
createOpContext (OpSendStatusFromServer m code) =
  OpSendStatusFromServerContext
  <$> C.createMetadata m
  <*> return (M.size m)
  <*> return code
createOpContext OpRecvInitialMetadata =
  fmap OpRecvInitialMetadataContext C.metadataArrayCreate
createOpContext OpRecvMessage =
  fmap OpRecvMessageContext C.createReceivingByteBuffer
createOpContext OpRecvStatusOnClient =
  OpRecvStatusOnClientContext
  <$> C.metadataArrayCreate
  <*> C.createStatusCodePtr
createOpContext OpRecvCloseOnServer =
  fmap OpRecvCloseOnServerContext $ malloc

-- | Mutates the given raw array of ops at the given index according to the
-- given 'OpContext'.
setOpArray :: C.OpArray -> Int -> OpContext -> IO ()
setOpArray arr i (OpSendInitialMetadataContext kvs l) =
  C.opSendInitialMetadata arr i kvs l
setOpArray arr i (OpSendMessageContext bb) =
  C.opSendMessage arr i bb
setOpArray arr i OpSendCloseFromClientContext =
  C.opSendCloseClient arr i
setOpArray arr i (OpSendStatusFromServerContext kvs l code) =
  C.opSendStatusServer arr i l kvs code "" --TODO: Issue #6
setOpArray arr i (OpRecvInitialMetadataContext pmetadata) =
  C.opRecvInitialMetadata arr i pmetadata
setOpArray arr i (OpRecvMessageContext pbb) =
  C.opRecvMessage arr i pbb
setOpArray arr i (OpRecvStatusOnClientContext pmetadata pstatus) = do
  pCString <- malloc --TODO: Issue #6
  C.opRecvStatusClient arr i pmetadata pstatus pCString 0
setOpArray arr i (OpRecvCloseOnServerContext pcancelled) = do
  C.opRecvCloseServer arr i pcancelled

-- | Cleans up an 'OpContext'.
freeOpContext :: OpContext -> IO ()
freeOpContext (OpSendInitialMetadataContext m _) = C.metadataFree m
freeOpContext (OpSendMessageContext bb) = C.grpcByteBufferDestroy bb
freeOpContext OpSendCloseFromClientContext = return ()
freeOpContext (OpSendStatusFromServerContext metadata _ _) =
  C.metadataFree metadata
freeOpContext (OpRecvInitialMetadataContext metadata) =
  C.metadataArrayDestroy metadata
freeOpContext (OpRecvMessageContext pbb) =
  C.destroyReceivingByteBuffer pbb
freeOpContext (OpRecvStatusOnClientContext metadata pcode) =
  C.metadataArrayDestroy metadata
  >> C.destroyStatusCodePtr pcode
freeOpContext (OpRecvCloseOnServerContext pcancelled) =
  grpcDebug ("freeOpContext: freeing pcancelled: " ++ show pcancelled)
  >> free pcancelled

-- | Converts a list of 'Op's into the corresponding 'OpContext's and guarantees
-- they will be cleaned up correctly.
withOpContexts :: [Op] -> ([OpContext] -> IO a) -> IO a
withOpContexts ops = bracket (mapM createOpContext ops)
                             (mapM freeOpContext)

withOpArray :: Int -> (C.OpArray -> IO a) -> IO a
withOpArray n = bracket (C.opArrayCreate n)
                        (flip C.opArrayDestroy n)

-- | Container holding GC-managed results for 'Op's which receive data.
data OpRecvResult =
  OpRecvInitialMetadataResult MetadataMap
  | OpRecvMessageResult B.ByteString
  | OpRecvStatusOnClientResult MetadataMap C.StatusCode
  | OpRecvCloseOnServerResult Bool -- ^ True if call was cancelled.
  deriving (Eq, Show)

-- | For the given 'OpContext', if the 'Op' receives data, copies the data out
-- of the 'OpContext' and into GC-managed Haskell types. After this, it is safe
-- to destroy the 'OpContext'.
resultFromOpContext :: OpContext -> IO (Maybe OpRecvResult)
resultFromOpContext (OpRecvInitialMetadataContext pmetadata) = do
  grpcDebug "resultFromOpContext: OpRecvInitialMetadataContext"
  metadata <- peek pmetadata
  metadataMap <- C.getAllMetadataArray metadata
  return $ Just $ OpRecvInitialMetadataResult metadataMap
resultFromOpContext (OpRecvMessageContext pbb) = do
  grpcDebug "resultFromOpContext: OpRecvMessageContext"
  bb <- peek pbb
  grpcDebug "resultFromOpContext: bytebuffer peeked."
  bs <- C.copyByteBufferToByteString bb
  grpcDebug "resultFromOpContext: bb copied."
  return $ Just $ OpRecvMessageResult bs
resultFromOpContext (OpRecvStatusOnClientContext pmetadata pcode) = do
  grpcDebug "resultFromOpContext: OpRecvStatusOnClientContext"
  metadata <- peek pmetadata
  metadataMap <- C.getAllMetadataArray metadata
  code <- C.derefStatusCodePtr pcode
  return $ Just $ OpRecvStatusOnClientResult metadataMap code
resultFromOpContext (OpRecvCloseOnServerContext pcancelled) = do
  grpcDebug "resultFromOpContext: OpRecvCloseOnServerContext"
  cancelled <- fmap (\x -> if x > 0 then True else False)
                    (peek pcancelled)
  return $ Just $ OpRecvCloseOnServerResult cancelled
resultFromOpContext _ = do
  grpcDebug "resultFromOpContext: saw non-result op type."
  return Nothing

--TODO: the list of 'Op's type is less specific than it could be. There are only
-- a few different sequences of 'Op's we will see in practice. Once we figure
-- out what those are, we should create a more specific sum type. This will also
-- allow us to make a more specific sum type to replace @[OpRecvResult]@, too.

-- | For a given call, run the given 'Op's on the given completion queue with
-- the given tag. Blocks until the ops are complete or the given number of
-- seconds have elapsed.
runOps :: Call
          -- ^ 'Call' that this batch is associated with. One call can be
          -- associated with many batches.
          -> CompletionQueue
          -- ^ Queue on which our tag will be placed once our ops are done
          -- running.
          -> [Op]
          -> TimeoutSeconds
          -- ^ How long to block waiting for the tag to appear on the queue.
          -- If we time out, the result of this action will be
          -- @CallBatchError BatchTimeout@.
          -> IO (Either GRPCIOError [OpRecvResult])
runOps call cq ops timeLimit =
  let l = length ops in
    withOpArray l $ \opArray -> do
      grpcDebug "runOps: created op array."
      withOpContexts ops $ \contexts -> do
        grpcDebug "runOps: allocated op contexts."
        sequence_ $ zipWith (setOpArray opArray) [0..l-1] contexts
        tag <- newTag cq
        callError <- startBatch cq (internalCall call) opArray l tag
        grpcDebug $ "runOps: called start_batch. callError: "
                     ++ (show callError)
        case callError of
          Left x -> return $ Left x
          Right () -> do
            ev <- pluck cq tag timeLimit
            grpcDebug $ "runOps: pluck returned " ++ show ev
            case ev of
              Right () -> do
                grpcDebug "runOps: got good op; starting."
                fmap (Right . catMaybes) $ mapM resultFromOpContext contexts
              Left err -> return $ Left err
