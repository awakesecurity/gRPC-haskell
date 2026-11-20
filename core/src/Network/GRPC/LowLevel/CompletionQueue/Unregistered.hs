{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ViewPatterns #-}

module Network.GRPC.LowLevel.CompletionQueue.Unregistered where

import Control.Exception (mask_)
import Foreign.Marshal.Alloc (free, malloc)
import Foreign.Storable (peek)
import Network.GRPC.LowLevel.Call
import qualified Network.GRPC.LowLevel.Call.Unregistered as U
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
  withPermission cq $ do
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

serverRequestCall ::
  C.Server ->
  -- | server CQ / notification CQ
  CompletionQueue ->
  -- | call CQ
  CompletionQueue ->
  IO (Either GRPCIOError U.ServerCall)
serverRequestCall s scq ccq = mask_ $ do
  call <- malloc
  meta <- C.metadataArrayCreate
  cd <- C.createCallDetails
  md <- peek meta
  let release = free call >> C.metadataArrayDestroy meta >> C.destroyCallDetails cd
      build =
        U.ServerCall
          <$> peek call
          <*> return ccq
          <*> C.getAllMetadataArray md
          <*> (C.timeSpec <$> C.callDetailsGetDeadline cd)
          <*> (MethodName <$> C.callDetailsGetMethod cd)
          <*> (Host <$> C.callDetailsGetHost cd)
  requestCall
    scq
    call
    release
    ( \tag -> do
        dbug $ "registering call for tag=" ++ show tag
        C.grpcServerRequestCall s call cd md (unsafeCQ ccq) (unsafeCQ scq) tag
    )
    build
  where
    dbug = grpcDebug . ("serverRequestCall(U): " ++)
