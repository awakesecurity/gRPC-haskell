{-# LANGUAGE RecordWildCards #-}

module Network.GRPC.LowLevel.Call.Unregistered where

import           Control.Monad
import           Foreign.Marshal.Alloc        (free)
import           Foreign.Ptr                  (Ptr)
#ifdef DEBUG
import           Foreign.Storable             (peek)
#endif
import           System.Clock                 (TimeSpec)

import           Network.GRPC.LowLevel.Call   (Host (..), MethodName (..))
import           Network.GRPC.LowLevel.GRPC   (MetadataMap, grpcDebug)
import qualified Network.GRPC.Unsafe          as C
import qualified Network.GRPC.Unsafe.Op as C

-- | Represents one unregistered GRPC call on the server.
-- Contains pointers to all the C state needed to respond to an unregistered
-- call.
data ServerCall = ServerCall
  { unServerCall        :: C.Call
  , requestMetadataRecv :: MetadataMap
  , parentPtr           :: Maybe (Ptr C.Call)
  , callDeadline        :: TimeSpec
  , callMethod          :: MethodName
  , callHost            :: Host
  }

serverCallCancel :: ServerCall -> C.StatusCode -> String -> IO ()
serverCallCancel sc code reason =
  C.grpcCallCancelWithStatus (unServerCall sc) code reason C.reserved

debugServerCall :: ServerCall -> IO ()
#ifdef DEBUG
debugServerCall ServerCall{..} = do
  let C.Call ptr = unServerCall
  grpcDebug $ "debugServerCall(U): server call: " ++ show ptr
  grpcDebug $ "debugServerCall(U): metadata: "
              ++ show requestMetadataRecv
  forM_ parentPtr $ \parentPtr' -> do
    grpcDebug $ "debugServerCall(U): parent ptr: " ++ show parentPtr'
    C.Call parent <- peek parentPtr'
    grpcDebug $ "debugServerCall(U): parent: " ++ show parent
  grpcDebug $ "debugServerCall(U): deadline: " ++ show callDeadline
  grpcDebug $ "debugServerCall(U): method: " ++ show callMethod
  grpcDebug $ "debugServerCall(U): host: " ++ show callHost
#else
{-# INLINE debugServerCall #-}
debugServerCall = const $ return ()
#endif

destroyServerCall :: ServerCall -> IO ()
destroyServerCall call@ServerCall{..} = do
  grpcDebug "destroyServerCall(U): entered."
  debugServerCall call
  grpcDebug $ "Destroying server-side call object: " ++ show unServerCall
  C.grpcCallDestroy unServerCall
  grpcDebug $ "freeing parentPtr: " ++ show parentPtr
  forM_ parentPtr free
