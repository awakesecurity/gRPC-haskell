{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ViewPatterns #-}

module Network.GRPC.LowLevel.CompletionQueue.Unregistered where

import Foreign.Marshal.Alloc (free, malloc)
import Foreign.Ptr (Ptr)
import Foreign.Storable (peek)
import Network.GRPC.LowLevel.Call
import qualified Network.GRPC.LowLevel.Call.Unregistered as U
import Network.GRPC.LowLevel.CompletionQueue (serverCallOperation)
import Network.GRPC.LowLevel.CompletionQueue.Internal
import Network.GRPC.LowLevel.GRPC
import qualified Network.GRPC.Unsafe as C
import qualified Network.GRPC.Unsafe.Constants as C
import qualified Network.GRPC.Unsafe.Metadata as C
import qualified Network.GRPC.Unsafe.Time as C

channelCreateCall ::
  C.Channel ->
  C.Call ->
  C.PropagationMask ->
  CompletionQueue ->
  MethodName ->
  Endpoint ->
  C.CTimeSpecPtr ->
  IO (Either GRPCIOError ClientCall)
channelCreateCall chan parent mask cq@CompletionQueue{..} meth endpt deadline =
  withPushPermission cq $ do
    call <-
      C.grpcChannelCreateCall
        chan
        parent
        mask
        unsafeCQ
        (unMethodName meth)
        (unEndpoint endpt)
        deadline
        C.reserved
    return $ Right $ ClientCall call

-- | Wait for an unregistered call. Blocks until a call arrives or the server
-- shuts down, in which case the result is 'GRPCIOShutdown'.
--
-- If the calling thread is interrupted while waiting, the request stays
-- registered with gRPC (it cannot be withdrawn); the call that eventually
-- satisfies it is cancelled.
serverRequestCall ::
  C.Server ->
  -- | server CQ / notification CQ
  CompletionQueue ->
  -- | call CQ
  CompletionQueue ->
  IO (Either GRPCIOError U.ServerCall)
serverRequestCall s scq ccq = runCompletion $ do
  -- NB: These are owned by the completion callback, not by this thread: gRPC
  -- writes to them when a call arrives, however long that takes.
  call <- malloc :: IO (Ptr C.Call)
  meta <- C.metadataArrayCreate
  cd <- C.createCallDetails
  let release = do
        free call
        C.metadataArrayDestroy meta
        C.destroyCallDetails cd
      build =
        U.ServerCall
          <$> peek call
          <*> return ccq
          <*> (peek meta >>= C.getAllMetadataArray)
          <*> (C.timeSpec <$> C.callDetailsGetDeadline cd)
          <*> (MethodName <$> C.callDetailsGetMethod cd)
          <*> (Host <$> C.callDetailsGetHost cd)
      request tag = do
        md <- peek meta
        dbug $ "registering call for tag=" ++ show tag
        ce <- C.grpcServerRequestCall s call cd md (unsafeCQ ccq) (unsafeCQ scq) tag
        dbug $ "request returned: " ++ show ce
        return ce
  return (serverCallOperation scq U.unsafeSC release build request)
  where
    dbug = grpcDebug . ("serverRequestCall(U): " ++)
