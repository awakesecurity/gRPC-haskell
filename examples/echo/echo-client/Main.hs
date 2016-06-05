{-# LANGUAGE OverloadedStrings #-}

import Control.Concurrent (threadDelay)
import Control.Monad (forever)
import Data.ByteString ()
import qualified Data.Map as M
import Network.GRPC.LowLevel

echoMethod :: MethodName
echoMethod = MethodName "/echo.Echo/DoEcho"

ntimes :: Int -> IO () -> IO ()
ntimes 1 f = f
ntimes n f = f >> (ntimes (n-1) f)

unregClient :: IO ()
unregClient = do
  withGRPC $ \grpc ->
    withClient grpc (ClientConfig "localhost" 50051) $ \client ->
    ntimes 100000 $ do
      reqResult <- clientRequest client echoMethod "localhost:50051" 1 "hi" M.empty
      case reqResult of
        Left x -> error $ "Got client error: " ++ show x
        Right resp -> return ()

regClient :: IO ()
regClient = do
  withGRPC $ \grpc ->
    withClient grpc (ClientConfig "localhost" 50051) $ \client -> ntimes 100000 $ do
      regMethod <- clientRegisterMethod client echoMethod Normal
      reqResult <- clientRegisteredRequest client regMethod 1 "hi" M.empty
      case reqResult of
        Left x -> error $ "Got client error: " ++ show x
        Right resp -> return ()

main :: IO ()
main = regClient
