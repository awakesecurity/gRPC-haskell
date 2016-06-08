{-# LANGUAGE RecordWildCards #-}

module Network.GRPC.LowLevel.Call.Unregistered where

import           Control.Monad
import           Foreign.Marshal.Alloc          (free)
import           Foreign.Ptr                    (Ptr)
import           Foreign.Storable               (peek)

import qualified Network.GRPC.Unsafe            as C
import qualified Network.GRPC.Unsafe.Metadata   as C

import           Network.GRPC.LowLevel.Call
import           Network.GRPC.LowLevel.GRPC     (MetadataMap, grpcDebug)

-- | Represents one unregistered GRPC call on the server.
-- Contains pointers to all the C state needed to respond to an unregistered
-- call.
data ServerCall = ServerCall
  { internalServerCall  :: C.Call
  , requestMetadataRecv :: Ptr C.MetadataArray
  , parentPtr           :: Maybe (Ptr C.Call)
  , callDetails         :: C.CallDetails
  }

serverCallGetMetadata :: ServerCall -> IO MetadataMap
serverCallGetMetadata ServerCall{..} = do
  marray <- peek requestMetadataRecv
  C.getAllMetadataArray marray

serverCallGetMethodName :: ServerCall -> IO MethodName
serverCallGetMethodName ServerCall{..} =
  MethodName <$> C.callDetailsGetMethod callDetails

serverCallGetHost :: ServerCall -> IO Host
serverCallGetHost ServerCall{..} =
  Host <$> C.callDetailsGetHost callDetails

debugServerCall :: ServerCall -> IO ()
#ifdef DEBUG
debugServerCall call@(ServerCall (C.Call ptr) _ _ _) = do
  grpcDebug $ "debugServerCall(U): server call: " ++ (show ptr)
  grpcDebug $ "debugServerCall(U): metadata ptr: "
              ++ show (requestMetadataRecv call)
  metadataArr <- peek (requestMetadataRecv call)
  metadata <- C.getAllMetadataArray metadataArr
  grpcDebug $ "debugServerCall(U): metadata received: " ++ (show metadata)
  forM_ (parentPtr call) $ \parentPtr' -> do
    grpcDebug $ "debugServerCall(U): parent ptr: " ++ show parentPtr'
    (C.Call parent) <- peek parentPtr'
    grpcDebug $ "debugServerCall(U): parent: " ++ show parent
  grpcDebug $ "debugServerCall(U): callDetails ptr: "
              ++ show (callDetails call)
  --TODO: need functions for getting data out of call_details.
#else
{-# INLINE debugServerCall #-}
debugServerCall = const $ return ()
#endif

destroyServerCall :: ServerCall -> IO ()
destroyServerCall call@ServerCall{..} = do
  grpcDebug "destroyServerCall(U): entered."
  debugServerCall call
  grpcDebug $ "Destroying server-side call object: "
              ++ show internalServerCall
  C.grpcCallDestroy internalServerCall
  grpcDebug $ "destroying metadata array: " ++ show requestMetadataRecv
  C.metadataArrayDestroy requestMetadataRecv
  grpcDebug $ "freeing parentPtr: " ++ show parentPtr
  forM_ parentPtr free
  grpcDebug $ "destroying call details: " ++ show callDetails
  C.destroyCallDetails callDetails
