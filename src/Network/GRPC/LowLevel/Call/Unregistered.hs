{-# LANGUAGE RecordWildCards #-}

module Network.GRPC.LowLevel.Call.Unregistered where

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

import           Network.GRPC.LowLevel.Call
import           Network.GRPC.LowLevel.GRPC     (MetadataMap, grpcDebug)

-- | Represents one unregistered GRPC call on the server.
-- Contains pointers to all the C state needed to respond to an unregistered
-- call.
data ServerUnregCall = ServerUnregCall
  {internalServerUnregCall :: C.Call,
   requestMetadataRecvUnreg :: Ptr C.MetadataArray,
   parentPtrUnreg :: Maybe (Ptr C.Call),
   callDetails :: C.CallDetails}

serverUnregCallGetMetadata :: ServerUnregCall -> IO MetadataMap
serverUnregCallGetMetadata ServerUnregCall{..} = do
  marray <- peek requestMetadataRecvUnreg
  C.getAllMetadataArray marray

serverUnregCallGetMethodName :: ServerUnregCall -> IO MethodName
serverUnregCallGetMethodName ServerUnregCall{..} =
  MethodName <$> C.callDetailsGetMethod callDetails

serverUnregCallGetHost :: ServerUnregCall -> IO Host
serverUnregCallGetHost ServerUnregCall{..} =
  Host <$> C.callDetailsGetHost callDetails

debugServerUnregCall :: ServerUnregCall -> IO ()
#ifdef DEBUG
debugServerUnregCall call@(ServerUnregCall (C.Call ptr) _ _ _) = do
  grpcDebug $ "debugServerUnregCall: server call: " ++ (show ptr)
  grpcDebug $ "debugServerUnregCall: metadata ptr: "
              ++ show (requestMetadataRecvUnreg call)
  metadataArr <- peek (requestMetadataRecvUnreg call)
  metadata <- C.getAllMetadataArray metadataArr
  grpcDebug $ "debugServerUnregCall: metadata received: " ++ (show metadata)
  forM_ (parentPtrUnreg call) $ \parentPtr' -> do
    grpcDebug $ "debugServerRegCall: parent ptr: " ++ show parentPtr'
    (C.Call parent) <- peek parentPtr'
    grpcDebug $ "debugServerRegCall: parent: " ++ show parent
  grpcDebug $ "debugServerUnregCall: callDetails ptr: "
              ++ show (callDetails call)
  --TODO: need functions for getting data out of call_details.
#else
{-# INLINE debugServerUnregCall #-}
debugServerUnregCall = const $ return ()
#endif

destroyServerUnregCall :: ServerUnregCall -> IO ()
destroyServerUnregCall call@ServerUnregCall{..} = do
  grpcDebug "destroyServerUnregCall: entered."
  debugServerUnregCall call
  grpcDebug $ "Destroying server-side call object: "
              ++ show internalServerUnregCall
  C.grpcCallDestroy internalServerUnregCall
  grpcDebug $ "destroying metadata array: " ++ show requestMetadataRecvUnreg
  C.metadataArrayDestroy requestMetadataRecvUnreg
  grpcDebug $ "freeing parentPtrUnreg: " ++ show parentPtrUnreg
  forM_ parentPtrUnreg free
  grpcDebug $ "destroying call details: " ++ show callDetails
  C.destroyCallDetails callDetails
