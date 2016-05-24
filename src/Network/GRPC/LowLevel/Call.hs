{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Network.GRPC.LowLevel.Call where

import           Control.Monad
import           Data.String (IsString)
import           Foreign.Marshal.Alloc (free)
import           Foreign.Ptr (Ptr, castPtr)
import           Foreign.Storable (peek)

import qualified Network.GRPC.Unsafe as C
import qualified Network.GRPC.Unsafe.Time as C
import qualified Network.GRPC.Unsafe.Metadata as C
import qualified Network.GRPC.Unsafe.ByteBuffer as C

import Network.GRPC.LowLevel.GRPC (grpcDebug)

-- | Models the four types of RPC call supported by gRPC. We currently only
-- support the first alternative, and only in a preliminary fashion.
data GRPCMethodType = Normal | ClientStreaming | ServerStreaming | BiDiStreaming
  deriving (Show, Eq, Ord, Enum)

newtype MethodName = MethodName {unMethodName :: String}
  deriving (Show, Eq, IsString)

newtype Host = Host {unHost :: String}
  deriving (Show, Eq, IsString)

-- | Represents a registered method. Methods can optionally be registered in
-- order to make the C-level request/response code simpler.
-- Before making or awaiting a registered call, the
-- method must be registered with the client (see 'clientRegisterMethod') and
-- the server (see 'serverRegisterMethod').
-- Contains state for identifying that method in the underlying gRPC library.
data RegisteredMethod = RegisteredMethod {methodType :: GRPCMethodType,
                                          methodName :: MethodName,
                                          methodHost :: Host,
                                          methodHandle :: C.CallHandle}

-- | Represents one GRPC call (i.e. request). This type is used on both the
-- client and server. Contains pointers to all the necessary C state needed to
-- send and respond to a call.
-- This is used to associate send/receive 'Op's with a request.
-- There are separate functions for creating these depending on whether the
-- method is registered and whether the call is on the client or server side.
data Call = ClientCall {internalCall :: C.Call}
            | ServerCall
            {internalCall :: C.Call,
             requestMetadataRecv :: (Ptr C.MetadataArray),
             optionalPayload :: Maybe (Ptr C.ByteBuffer),
             parentPtr :: Maybe (Ptr C.Call),
             callDetails :: Maybe (C.CallDetails),
             -- ^ used on the server for non-registered calls
             --, to identify the endpoint being used.
             callDeadline :: Maybe C.CTimeSpecPtr
            }

debugCall :: Call -> IO ()
#ifdef DEBUG
debugCall (ClientCall (C.Call ptr)) =
  grpcDebug $ "debugCall: client call: " ++ (show ptr)
debugCall call@(ServerCall (C.Call ptr) _ _ _ _ _) = do
  grpcDebug $ "debugCall: server call: " ++ (show ptr)
  grpcDebug $ "debugCall: metadata ptr: " ++ show (requestMetadataRecv call)
  metadataArr <- peek (requestMetadataRecv call)
  metadata <- C.getAllMetadataArray metadataArr
  grpcDebug $ "debugCall: metadata received: " ++ (show metadata)
  forM_ (optionalPayload call) $ \payloadPtr -> do
    grpcDebug $ "debugCall: payload ptr: " ++ show payloadPtr
    payload <- peek payloadPtr
    bs <- C.copyByteBufferToByteString payload
    grpcDebug $ "debugCall: payload contents: " ++ show bs
  forM_ (parentPtr call) $ \parentPtr' -> do
    grpcDebug $ "debugCall: parent ptr: " ++ show parentPtr'
    (C.Call parent) <- peek parentPtr'
    grpcDebug $ "debugCall: parent: " ++ show parent
  forM_ (callDetails call) $ \(C.CallDetails callDetailsPtr) -> do
    grpcDebug $ "debugCall: callDetails ptr: " ++ show callDetailsPtr
    --TODO: need functions for getting data out of call_details.
  forM_ (callDeadline call) $ \timespecptr -> do
    grpcDebug $ "debugCall: deadline ptr: " ++ show timespecptr
    timespec <- peek timespecptr
    grpcDebug $ "debugCall: deadline: " ++ show (C.timeSpec timespec)
#else
{-# INLINE debugCall #-}
debugCall = const $ return ()
#endif

-- | Destroys a 'Call'.
destroyCall :: Call -> IO ()
destroyCall ClientCall{..} = do
  grpcDebug "Destroying client-side call object."
  C.grpcCallDestroy internalCall
destroyCall call@ServerCall{..} = do
  grpcDebug "destroyCall: entered."
  debugCall call
  grpcDebug $ "Destroying server-side call object: " ++ show internalCall
  C.grpcCallDestroy internalCall
  grpcDebug $ "destroying metadata array: " ++ show requestMetadataRecv
  C.metadataArrayDestroy requestMetadataRecv
  grpcDebug $ "destroying optional payload" ++ show optionalPayload
  forM_ optionalPayload C.destroyReceivingByteBuffer
  grpcDebug $ "freeing parentPtr: " ++ show parentPtr
  forM_ parentPtr free
  grpcDebug $ "destroying call details" ++ show callDetails
  forM_ callDetails C.destroyCallDetails
  grpcDebug $ "destroying deadline." ++ show callDeadline
  forM_ callDeadline C.timespecDestroy
