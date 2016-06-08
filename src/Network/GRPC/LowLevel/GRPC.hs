{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Network.GRPC.LowLevel.GRPC where
import           Control.Concurrent                      (threadDelay)
import           Control.Exception
import qualified Data.ByteString as B
import qualified Data.Map as M
import           Data.String (IsString)
import qualified Network.GRPC.Unsafe as C
import qualified Network.GRPC.Unsafe.Op as C

#ifdef DEBUG
import           GHC.Conc (myThreadId)
#endif

type MetadataMap = M.Map B.ByteString B.ByteString

newtype StatusDetails = StatusDetails B.ByteString deriving (Show, Eq, IsString)

-- | Functions as a proof that the gRPC core has been started. The gRPC core
-- must be initialized to create any gRPC state, so this is a requirement for
-- the server and client create/start functions.
data GRPC = GRPC

withGRPC :: (GRPC -> IO a) -> IO a
withGRPC = bracket (C.grpcInit >> return GRPC) (const C.grpcShutdown)

-- | Describes all errors that can occur while running a GRPC-related IO action.
data GRPCIOError = GRPCIOCallError C.CallError
                   -- ^ Errors that can occur while the call is in flight. These
                   -- errors come from the core gRPC library directly.
                   | GRPCIOTimeout
                   -- ^ Indicates that we timed out while waiting for an
                   -- operation to complete on the 'CompletionQueue'.
                   | GRPCIOShutdown
                   -- ^ Indicates that the 'CompletionQueue' is shutting down
                   -- and no more work can be processed. This can happen if the
                   -- client or server is shutting down.
                   | GRPCIOShutdownFailure
                   -- ^ Thrown if a 'CompletionQueue' fails to shut down in a
                   -- reasonable amount of time.
                   | GRPCIOUnknownError
                   | GRPCIOBadStatusCode C.StatusCode StatusDetails
  deriving (Show, Eq)

throwIfCallError :: C.CallError -> Either GRPCIOError ()
throwIfCallError C.CallOk = Right ()
throwIfCallError x = Left $ GRPCIOCallError x

grpcDebug :: String -> IO ()
{-# INLINE grpcDebug #-}
#ifdef DEBUG
grpcDebug str = do tid <- myThreadId
                   putStrLn $ (show tid) ++ ": " ++ str
#else
grpcDebug _ = return ()
#endif

threadDelaySecs :: Int -> IO ()
threadDelaySecs = threadDelay . (* 10^(6::Int))

{-
-- TODO: remove this once finally decided on whether to use it.
-- | Monad for running gRPC operations.
newtype GRPCIO a = GRPCIO {unGRPCIO :: ExceptT GRPCIOError IO a}
  deriving (Functor, Applicative, Monad, MonadIO)

deriving instance MonadError GRPCIOError GRPCIO

runGRPCIO :: GRPCIO a -> IO (Either GRPCIOError a)
runGRPCIO = runExceptT . unGRPCIO

unrunGRPCIO :: IO (Either GRPCIOError a) -> GRPCIO a
unrunGRPCIO = GRPCIO . ExceptT

continueFrom :: (a -> GRPCIO b) -> (Either GRPCIOError a) -> GRPCIO b
continueFrom f (Left x) = throwError x
continueFrom f (Right x) = f x

wrapGRPC :: Either GRPCIOError a -> GRPCIO a
wrapGRPC (Left x) = throwError x
wrapGRPC (Right x) = return x

grpcBracket :: GRPCIO a -> (a -> GRPCIO b) -> (a -> GRPCIO c) -> GRPCIO c
grpcBracket create destroy f = unrunGRPCIO $ do
  let createAction = runGRPCIO create
  let fAction = runGRPCIO . continueFrom f
  let destroyAction = runGRPCIO . continueFrom destroy
  bracket createAction destroyAction fAction
-}
