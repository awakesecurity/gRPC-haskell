{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards            #-}

module Network.GRPC.LowLevel.Op where

import           Control.Exception
import qualified Data.ByteString                       as B
import qualified Data.Map.Strict                       as M
import           Data.Maybe                            (catMaybes)
import           Foreign.C.String                      (CString)
import           Foreign.C.Types                       (CInt)
import           Foreign.Marshal.Alloc                 (free, malloc,
                                                        mallocBytes)
import           Foreign.Ptr                           (Ptr, nullPtr)
import           Foreign.Storable                      (peek, poke)
import           Network.GRPC.LowLevel.Call
import           Network.GRPC.LowLevel.CompletionQueue
import           Network.GRPC.LowLevel.GRPC
import qualified Network.GRPC.Unsafe                   as C (Call)
import qualified Network.GRPC.Unsafe.ByteBuffer        as C
import qualified Network.GRPC.Unsafe.Metadata          as C
import qualified Network.GRPC.Unsafe.Op                as C
import qualified Network.GRPC.Unsafe.Slice             as C (Slice, freeSlice)

-- | Sum describing all possible send and receive operations that can be batched
-- and executed by gRPC. Usually these are processed in a handful of
-- combinations depending on the 'MethodType' of the call being run.
data Op = OpSendInitialMetadata MetadataMap
          | OpSendMessage B.ByteString
          | OpSendCloseFromClient
          | OpSendStatusFromServer MetadataMap C.StatusCode StatusDetails
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
  | OpSendMessageContext (C.ByteBuffer, C.Slice)
  | OpSendCloseFromClientContext
  | OpSendStatusFromServerContext C.MetadataKeyValPtr Int C.StatusCode
                                  B.ByteString
  | OpRecvInitialMetadataContext (Ptr C.MetadataArray)
  | OpRecvMessageContext (Ptr C.ByteBuffer)
  | OpRecvStatusOnClientContext (Ptr C.MetadataArray) (Ptr C.StatusCode)
                                (Ptr CString)
  | OpRecvCloseOnServerContext (Ptr CInt)
  deriving Show

-- | Length we pass to gRPC for receiving status details
-- when processing 'OpRecvStatusOnClient'. It appears that gRPC actually ignores
-- this length and reallocates a longer string if necessary.
defaultStatusStringLen :: Int
defaultStatusStringLen = 128

-- | Allocates and initializes the 'Opcontext' corresponding to the given 'Op'.
createOpContext :: Op -> IO OpContext
createOpContext (OpSendInitialMetadata m) =
  OpSendInitialMetadataContext
  <$> C.createMetadata m
  <*> return (M.size m)
createOpContext (OpSendMessage bs) =
  fmap OpSendMessageContext (C.createByteBuffer bs)
createOpContext (OpSendCloseFromClient) = return OpSendCloseFromClientContext
createOpContext (OpSendStatusFromServer m code (StatusDetails str)) =
  OpSendStatusFromServerContext
  <$> C.createMetadata m
  <*> return (M.size m)
  <*> return code
  <*> return str
createOpContext OpRecvInitialMetadata =
  fmap OpRecvInitialMetadataContext C.metadataArrayCreate
createOpContext OpRecvMessage =
  fmap OpRecvMessageContext C.createReceivingByteBuffer
createOpContext OpRecvStatusOnClient = do
  pmetadata <- C.metadataArrayCreate
  pstatus <- C.createStatusCodePtr
  pstr <- malloc
  cstring <- mallocBytes defaultStatusStringLen
  poke pstr cstring
  return $ OpRecvStatusOnClientContext pmetadata pstatus pstr
createOpContext OpRecvCloseOnServer =
  fmap OpRecvCloseOnServerContext $ malloc

-- | Mutates the given raw array of ops at the given index according to the
-- given 'OpContext'.
setOpArray :: C.OpArray -> Int -> OpContext -> IO ()
setOpArray arr i (OpSendInitialMetadataContext kvs l) =
  C.opSendInitialMetadata arr i kvs l
setOpArray arr i (OpSendMessageContext (bb,_)) =
  C.opSendMessage arr i bb
setOpArray arr i OpSendCloseFromClientContext =
  C.opSendCloseClient arr i
setOpArray arr i (OpSendStatusFromServerContext kvs l code details) =
  B.useAsCString details $ \cstr ->
    C.opSendStatusServer arr i l kvs code cstr
setOpArray arr i (OpRecvInitialMetadataContext pmetadata) =
  C.opRecvInitialMetadata arr i pmetadata
setOpArray arr i (OpRecvMessageContext pbb) =
  C.opRecvMessage arr i pbb
setOpArray arr i (OpRecvStatusOnClientContext pmetadata pstatus pstr) = do
  C.opRecvStatusClient arr i pmetadata pstatus pstr defaultStatusStringLen
setOpArray arr i (OpRecvCloseOnServerContext pcancelled) = do
  C.opRecvCloseServer arr i pcancelled

-- | Cleans up an 'OpContext'.
freeOpContext :: OpContext -> IO ()
freeOpContext (OpSendInitialMetadataContext m _) = C.metadataFree m
freeOpContext (OpSendMessageContext (bb, s)) =
  C.grpcByteBufferDestroy bb >> C.freeSlice s
freeOpContext OpSendCloseFromClientContext = return ()
freeOpContext (OpSendStatusFromServerContext metadata _ _ _) =
  C.metadataFree metadata
freeOpContext (OpRecvInitialMetadataContext metadata) =
  C.metadataArrayDestroy metadata
freeOpContext (OpRecvMessageContext pbb) =
  C.destroyReceivingByteBuffer pbb
freeOpContext (OpRecvStatusOnClientContext metadata pcode pstr) = do
  C.metadataArrayDestroy metadata
  C.destroyStatusCodePtr pcode
  str <- peek pstr
  free str
  free pstr
freeOpContext (OpRecvCloseOnServerContext pcancelled) =
  grpcDebug ("freeOpContext: freeing pcancelled: " ++ show pcancelled)
  >> free pcancelled

-- | Allocates an `OpArray` and a list of `OpContext`s from the given list of
-- `Op`s.
withOpArrayAndCtxts :: [Op] -> ((C.OpArray, [OpContext]) -> IO a) -> IO a
withOpArrayAndCtxts ops = bracket setup teardown
  where setup = do ctxts <- mapM createOpContext ops
                   let l = length ops
                   arr <- C.opArrayCreate l
                   sequence_ $ zipWith (setOpArray arr) [0..l-1] ctxts
                   return (arr, ctxts)
        teardown (arr, ctxts) = do C.opArrayDestroy arr (length ctxts)
                                   mapM_ freeOpContext ctxts

-- | Container holding GC-managed results for 'Op's which receive data.
data OpRecvResult =
  OpRecvInitialMetadataResult MetadataMap
  | OpRecvMessageResult (Maybe B.ByteString)
    -- ^ If the client or server dies, we might not receive a response body, in
    -- which case this will be 'Nothing'.
  | OpRecvStatusOnClientResult MetadataMap C.StatusCode B.ByteString
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
  bb@(C.ByteBuffer bbptr) <- peek pbb
  if bbptr == nullPtr
     then return $ Just $ OpRecvMessageResult Nothing
     else do bs <- C.copyByteBufferToByteString bb
             grpcDebug "resultFromOpContext: bb copied."
             return $ Just $ OpRecvMessageResult (Just bs)
resultFromOpContext (OpRecvStatusOnClientContext pmetadata pcode pstr) = do
  grpcDebug "resultFromOpContext: OpRecvStatusOnClientContext"
  metadata <- peek pmetadata
  metadataMap <- C.getAllMetadataArray metadata
  code <- C.derefStatusCodePtr pcode
  cstr <- peek pstr
  statusInfo <- B.packCString cstr
  return $ Just $ OpRecvStatusOnClientResult metadataMap code statusInfo
resultFromOpContext (OpRecvCloseOnServerContext pcancelled) = do
  grpcDebug "resultFromOpContext: OpRecvCloseOnServerContext"
  cancelled <- fmap (\x -> if x > 0 then True else False)
                    (peek pcancelled)
  return $ Just $ OpRecvCloseOnServerResult cancelled
resultFromOpContext _ = do
  grpcDebug "resultFromOpContext: saw non-result op type."
  return Nothing

-- | For a given call, run the given 'Op's on the given completion queue with
-- the given tag. Blocks until the ops are complete or the deadline on the
-- associated call has been reached.
-- TODO: now that we distinguish between different types
-- of calls at the type level, we could try to limit the input 'Op's more
-- appropriately. E.g., we don't use an 'OpRecvInitialMetadata' when receiving a
-- registered call, because gRPC handles that for us.

-- TODO: the list of 'Op's type is less specific than it could be. There are
-- only a few different sequences of 'Op's we will see in practice. Once we
-- figure out what those are, we should create a more specific sum
-- type. However, since ops can fail, the list of 'OpRecvResult' returned by
-- 'runOps' can vary in their contents and are perhaps less amenable to
-- simplification.  In the meantime, from looking at the core tests, it looks
-- like it is safe to say that we always get a
-- GRPC_CALL_ERROR_TOO_MANY_OPERATIONS error if we use the same 'Op' twice in
-- the same batch, so we might want to change the list to a set. I don't think
-- order matters within a batch. Need to check.

runOps :: C.Call
          -- ^ 'Call' that this batch is associated with. One call can be
          -- associated with many batches.
       -> CompletionQueue
          -- ^ Queue on which our tag will be placed once our ops are done
          -- running.
       -> [Op]
          -- ^ The list of 'Op's to execute.
       -> IO (Either GRPCIOError [OpRecvResult])
runOps call cq ops =
  let l = length ops in
    withOpArrayAndCtxts ops $ \(opArray, contexts) -> do
      grpcDebug $ "runOps: allocated op contexts: " ++ show contexts
      tag <- newTag cq
      grpcDebug $ "runOps: tag: " ++ show tag
      callError <- startBatch cq call opArray l tag
      grpcDebug $ "runOps: called start_batch. callError: "
                   ++ (show callError)
      case callError of
        Left x -> return $ Left x
        Right () -> do
          ev <- pluck cq tag Nothing
          grpcDebug $ "runOps: pluck returned " ++ show ev
          case ev of
            Right () -> do
              grpcDebug "runOps: got good op; starting."
              fmap (Right . catMaybes) $ mapM resultFromOpContext contexts
            Left err -> return $ Left err

-- | If response status info is present in the given 'OpRecvResult's, returns
-- a tuple of trailing metadata, status code, and status details.
extractStatusInfo :: [OpRecvResult]
                     -> Maybe (MetadataMap, C.StatusCode, B.ByteString)
extractStatusInfo [] = Nothing
extractStatusInfo (OpRecvStatusOnClientResult meta code details:_) =
  Just (meta, code, details)
extractStatusInfo (_:xs) = extractStatusInfo xs
