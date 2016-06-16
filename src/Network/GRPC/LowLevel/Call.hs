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
import           System.Clock

import qualified Network.GRPC.Unsafe            as C
import qualified Network.GRPC.Unsafe.ByteBuffer as C
import qualified Network.GRPC.Unsafe.Metadata   as C
import qualified Network.GRPC.Unsafe.Time       as C
import qualified Network.GRPC.Unsafe.Op         as C

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
data ClientCall = ClientCall { unClientCall :: C.Call }

clientCallCancel :: ClientCall -> IO ()
clientCallCancel cc = C.grpcCallCancel (unClientCall cc) C.reserved

-- | Represents one registered GRPC call on the server. Contains pointers to all
-- the C state needed to respond to a registered call.
data ServerCall = ServerCall
  { unServerCall        :: C.Call,
    requestMetadataRecv :: Ptr C.MetadataArray,
    optionalPayload     :: Ptr C.ByteBuffer,
    parentPtr           :: Maybe (Ptr C.Call),
    callDeadlinePtr     :: C.CTimeSpecPtr,
    callDeadline        :: TimeSpec
  }

serverCallCancel :: ServerCall -> C.StatusCode -> String -> IO ()
serverCallCancel sc code reason =
  C.grpcCallCancelWithStatus (unServerCall sc) code reason C.reserved

serverCallGetMetadata :: ServerCall -> IO MetadataMap
serverCallGetMetadata ServerCall{..} = do
  marray <- peek requestMetadataRecv
  C.getAllMetadataArray marray

-- | Extract the client request body from the given call, if present.  TODO: the
-- reason this returns @Maybe ByteString@ is because the gRPC library calls the
-- underlying out parameter "optional_payload". I am not sure exactly in what
-- cases it won't be present. The C++ library checks a has_request_payload_ bool
-- and passes in nullptr to request_registered_call if the bool is false, so we
-- may be able to do the payload present/absent check earlier.
serverCallGetPayload :: ServerCall -> IO (Maybe ByteString)
serverCallGetPayload ServerCall{..} = do
  bb@(C.ByteBuffer rawPtr) <- peek optionalPayload
  if rawPtr == nullPtr
     then return Nothing
     else Just <$> C.copyByteBufferToByteString bb

serverCallIsExpired :: ServerCall -> IO Bool
serverCallIsExpired sc = do
  currTime <- getTime Monotonic
  return $ currTime > (callDeadline sc)

debugClientCall :: ClientCall -> IO ()
{-# INLINE debugClientCall #-}
#ifdef DEBUG
debugClientCall (ClientCall (C.Call ptr)) =
  grpcDebug $ "debugCall: client call: " ++ (show ptr)
#else
debugClientCall = const $ return ()
#endif

debugServerCall :: ServerCall -> IO ()
#ifdef DEBUG
debugServerCall call@(ServerCall (C.Call ptr) _ _ _ _ _) = do
  grpcDebug $ "debugServerCall(R): server call: " ++ (show ptr)
  grpcDebug $ "debugServerCall(R): metadata ptr: "
              ++ show (requestMetadataRecv call)
  metadataArr <- peek (requestMetadataRecv call)
  metadata <- C.getAllMetadataArray metadataArr
  grpcDebug $ "debugServerCall(R): metadata received: " ++ (show metadata)
  grpcDebug $ "debugServerCall(R): payload ptr: " ++ show (optionalPayload call)
  payload <- peek (optionalPayload call)
  bs <- C.copyByteBufferToByteString payload
  grpcDebug $ "debugServerCall(R): payload contents: " ++ show bs
  forM_ (parentPtr call) $ \parentPtr' -> do
    grpcDebug $ "debugServerCall(R): parent ptr: " ++ show parentPtr'
    (C.Call parent) <- peek parentPtr'
    grpcDebug $ "debugServerCall(R): parent: " ++ show parent
  grpcDebug $ "debugServerCall(R): deadline ptr: " ++ show (callDeadline call)
  timespec <- peek (callDeadlinePtr call)
  grpcDebug $ "debugServerCall(R): deadline: " ++ show (C.timeSpec timespec)
#else
{-# INLINE debugServerCall #-}
debugServerCall = const $ return ()
#endif

destroyClientCall :: ClientCall -> IO ()
destroyClientCall ClientCall{..} = do
  grpcDebug "Destroying client-side call object."
  C.grpcCallDestroy unClientCall

destroyServerCall :: ServerCall -> IO ()
destroyServerCall call@ServerCall{..} = do
  grpcDebug "destroyServerCall(R): entered."
  debugServerCall call
  grpcDebug $ "Destroying server-side call object: " ++ show unServerCall
  C.grpcCallDestroy unServerCall
  grpcDebug $ "destroying metadata array: " ++ show requestMetadataRecv
  C.metadataArrayDestroy requestMetadataRecv
  grpcDebug $ "destroying optional payload" ++ show optionalPayload
  C.destroyReceivingByteBuffer optionalPayload
  grpcDebug $ "freeing parentPtr: " ++ show parentPtr
  forM_ parentPtr free
  grpcDebug $ "destroying deadline." ++ show callDeadline
  C.timespecDestroy callDeadlinePtr
