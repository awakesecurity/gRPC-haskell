{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving #-}

module Network.GRPC.LowLevel.GRPC (
  GRPC,
  withGRPC,
  startGRPC,
  stopGRPC,
  GRPCIOError (..),
  throwIfCallError,
  grpcDebug,
  grpcDebug',
  threadDelaySecs,
  MetadataMap (..),
  C.StatusDetails (..),
) where

import Control.Concurrent (myThreadId, threadDelay)
import Control.Exception
import Data.Functor (($>))
import Data.Typeable
import Network.GRPC.LowLevel.GRPC.MetadataMap (MetadataMap (..))
import qualified Network.GRPC.Unsafe as C
import qualified Network.GRPC.Unsafe.Op as C

-- | Functions as a proof that the gRPC core has been started. The gRPC core
-- must be initialized to create any gRPC state, so this is a requirement for
-- the server and client create/start functions.
data GRPC = GRPC

withGRPC :: (GRPC -> IO a) -> IO a
withGRPC = bracket startGRPC stopGRPC

-- | Start gRPC core and obtain a 'GRPC' witness. This function does not perform
-- any cleanup once the gRPC server is no longer needed.
--
-- Where possible, consider using 'withGRPC' which handles shutdown of gRPC
-- automatically with 'bracket'.
startGRPC :: IO GRPC
startGRPC = C.grpcInit $> GRPC

-- | Shutdown gRPC core given a 'GRPC' witnessing that gRPC core has been
-- initialized.
stopGRPC :: GRPC -> IO ()
stopGRPC GRPC = do
  grpcDebug "withGRPC: shutting down"
  C.grpcShutdown

-- | Describes all errors that can occur while running a GRPC-related IO
-- action.
data GRPCIOError
  = -- | Errors that can occur while the call is in flight. These
    -- errors come from the core gRPC library directly.
    GRPCIOCallError C.CallError
  | -- | Indicates that we timed out while waiting for an
    -- operation to complete on the 'CompletionQueue'.
    GRPCIOTimeout
  | -- | Indicates that the 'CompletionQueue' is shutting down
    -- and no more work can be processed. This can happen if the
    -- client or server is shutting down.
    GRPCIOShutdown
  | -- | Thrown if a 'CompletionQueue' fails to shut down in a
    -- reasonable amount of time.
    GRPCIOShutdownFailure
  | GRPCIOUnknownError
  | GRPCIOBadStatusCode C.StatusCode C.StatusDetails
  | GRPCIODecodeError String
  | GRPCIOInternalUnexpectedRecv String -- debugging description
  | GRPCIOHandlerException String
  deriving (Eq, Show, Typeable)

instance Exception GRPCIOError

throwIfCallError :: C.CallError -> Either GRPCIOError ()
throwIfCallError C.CallOk = Right ()
throwIfCallError x = Left $ GRPCIOCallError x

grpcDebug :: String -> IO ()
{-# INLINE grpcDebug #-}
#ifdef DEBUG
grpcDebug = grpcDebug'
#else
grpcDebug _ = return ()
#endif

grpcDebug' :: String -> IO ()
{-# INLINE grpcDebug' #-}
grpcDebug' str = do
  tid <- myThreadId
  putStrLn $ "[" ++ show tid ++ "]: " ++ str

threadDelaySecs :: Int -> IO ()
threadDelaySecs = threadDelay . (* 10 ^ (6 :: Int))
