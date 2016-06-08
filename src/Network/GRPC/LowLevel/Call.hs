{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards            #-}

-- | This module defines data structures and operations pertaining to registered
-- calls; for unregistered call support, see
-- `Network.GRPC.LowLevel.Call.Unregistered`.
module Network.GRPC.LowLevel.Call where

import           Control.Monad
import           Data.ByteString                (ByteString)
import           Data.String                    (IsString)
import           Foreign.Marshal.Alloc          (free)
import           Foreign.Ptr                    (Ptr, nullPtr)
import           Foreign.Storable               (peek)

import qualified Network.GRPC.Unsafe            as C
import qualified Network.GRPC.Unsafe.ByteBuffer as C
import qualified Network.GRPC.Unsafe.Metadata   as C
import qualified Network.GRPC.Unsafe.Time       as C

import           Network.GRPC.LowLevel.GRPC     (MetadataMap, grpcDebug)

-- | Models the four types of RPC call supported by gRPC. We currently only
-- support the first alternative, and only in a preliminary fashion.
data GRPCMethodType = Normal | ClientStreaming | ServerStreaming | BiDiStreaming
  deriving (Show, Eq, Ord, Enum)

newtype MethodName = MethodName {unMethodName :: String}
  deriving (Show, Eq, IsString)

newtype Host = Host {unHost :: String}
  deriving (Show, Eq, IsString)

newtype Port = Port {unPort :: Int}
  deriving (Eq, Num, Show)

newtype Endpoint = Endpoint {unEndpoint :: String}
  deriving (Show, Eq, IsString)

-- | Given a hostname and port, produces a "host:port" string
endpoint :: Host -> Port -> Endpoint
endpoint (Host h) (Port p) = Endpoint (h ++ ":" ++ show p)

-- | Represents a registered method. Methods can optionally be registered in
-- order to make the C-level request/response code simpler.
-- Before making or awaiting a registered call, the
-- method must be registered with the client (see 'clientRegisterMethod') and
-- the server (see 'serverRegisterMethod').
-- Contains state for identifying that method in the underlying gRPC library.
data RegisteredMethod = RegisteredMethod {methodType :: GRPCMethodType,
                                          methodName :: MethodName,
                                          methodEndpoint :: Endpoint,
                                          methodHandle :: C.CallHandle}

-- | Represents one GRPC call (i.e. request) on the client.
-- This is used to associate send/receive 'Op's with a request.
data ClientCall = ClientCall {internalClientCall :: C.Call}

-- | Represents one registered GRPC call on the server.
--  Contains pointers to all the C state needed to respond to a registered call.
data ServerRegCall = ServerRegCall
  {internalServerRegCall :: C.Call,
   requestMetadataRecvReg :: Ptr C.MetadataArray,
   optionalPayload :: Ptr C.ByteBuffer,
   parentPtrReg :: Maybe (Ptr C.Call),
   callDeadline :: C.CTimeSpecPtr
  }

serverRegCallGetMetadata :: ServerRegCall -> IO MetadataMap
serverRegCallGetMetadata ServerRegCall{..} = do
  marray <- peek requestMetadataRecvReg
  C.getAllMetadataArray marray

-- | Extract the client request body from the given registered call, if present.
-- TODO: the reason this returns @Maybe ByteString@ is because the gRPC library
-- calls the underlying out parameter "optional_payload". I am not sure exactly
-- in what cases it won't be present. The C++ library checks a
-- has_request_payload_ bool and passes in nullptr to request_registered_call
-- if the bool is false, so we may be able to do the payload present/absent
-- check earlier.
serverRegCallGetPayload :: ServerRegCall -> IO (Maybe ByteString)
serverRegCallGetPayload ServerRegCall{..} = do
  bb@(C.ByteBuffer rawPtr) <- peek optionalPayload
  if rawPtr == nullPtr
     then return Nothing
     else Just <$> C.copyByteBufferToByteString bb

debugClientCall :: ClientCall -> IO ()
{-# INLINE debugClientCall #-}
#ifdef DEBUG
debugClientCall (ClientCall (C.Call ptr)) =
  grpcDebug $ "debugCall: client call: " ++ (show ptr)
#else
debugClientCall = const $ return ()
#endif

debugServerRegCall :: ServerRegCall -> IO ()
#ifdef DEBUG
debugServerRegCall call@(ServerRegCall (C.Call ptr) _ _ _ _) = do
  grpcDebug $ "debugServerRegCall: server call: " ++ (show ptr)
  grpcDebug $ "debugServerRegCall: metadata ptr: "
              ++ show (requestMetadataRecvReg call)
  metadataArr <- peek (requestMetadataRecvReg call)
  metadata <- C.getAllMetadataArray metadataArr
  grpcDebug $ "debugServerRegCall: metadata received: " ++ (show metadata)
  grpcDebug $ "debugServerRegCall: payload ptr: " ++ show (optionalPayload call)
  payload <- peek (optionalPayload call)
  bs <- C.copyByteBufferToByteString payload
  grpcDebug $ "debugServerRegCall: payload contents: " ++ show bs
  forM_ (parentPtrReg call) $ \parentPtr' -> do
    grpcDebug $ "debugServerRegCall: parent ptr: " ++ show parentPtr'
    (C.Call parent) <- peek parentPtr'
    grpcDebug $ "debugServerRegCall: parent: " ++ show parent
  grpcDebug $ "debugServerRegCall: deadline ptr: " ++ show (callDeadline call)
  timespec <- peek (callDeadline call)
  grpcDebug $ "debugServerRegCall: deadline: " ++ show (C.timeSpec timespec)
#else
{-# INLINE debugServerRegCall #-}
debugServerRegCall = const $ return ()
#endif

destroyClientCall :: ClientCall -> IO ()
destroyClientCall ClientCall{..} = do
  grpcDebug "Destroying client-side call object."
  C.grpcCallDestroy internalClientCall

destroyServerRegCall :: ServerRegCall -> IO ()
destroyServerRegCall call@ServerRegCall{..} = do
  grpcDebug "destroyServerRegCall: entered."
  debugServerRegCall call
  grpcDebug $ "Destroying server-side call object: "
              ++ show internalServerRegCall
  C.grpcCallDestroy internalServerRegCall
  grpcDebug $ "destroying metadata array: " ++ show requestMetadataRecvReg
  C.metadataArrayDestroy requestMetadataRecvReg
  grpcDebug $ "destroying optional payload" ++ show optionalPayload
  C.destroyReceivingByteBuffer optionalPayload
  grpcDebug $ "freeing parentPtr: " ++ show parentPtrReg
  forM_ parentPtrReg free
  grpcDebug $ "destroying deadline." ++ show callDeadline
  C.timespecDestroy callDeadline
