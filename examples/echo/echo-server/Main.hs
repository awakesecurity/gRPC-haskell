{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# OPTIONS_GHC -fno-warn-unused-binds       #-}

import           Control.Concurrent.Async                  (async, wait)
import           Control.Monad                             (forever)
import           Data.ByteString                           (ByteString)
import           Network.GRPC.LowLevel
import qualified Network.GRPC.LowLevel.Server.Unregistered as U
import qualified Network.GRPC.LowLevel.Call.Unregistered as U

serverMeta :: MetadataMap
serverMeta = [("test_meta", "test_meta_value")]

handler :: U.ServerCall
           -> ByteString
           -> IO (ByteString, MetadataMap, StatusCode, StatusDetails)
handler U.ServerCall{..} reqBody = do
  --putStrLn $ "Got request for method: " ++ show method
  --putStrLn $ "Got metadata: " ++ show reqMeta
  return (reqBody, serverMeta, StatusOk, StatusDetails "")

unregMain :: IO ()
unregMain = withGRPC $ \grpc -> do
  withServer grpc (ServerConfig "localhost" 50051 []) $ \server -> forever $ do
    result <- U.serverHandleNormalCall server serverMeta handler
    case result of
      Left x -> putStrLn $ "handle call result error: " ++ show x
      Right _ -> return ()

regMain :: IO ()
regMain = withGRPC $ \grpc -> do
  let methods = [(MethodName "/echo.Echo/DoEcho", Normal)]
  withServer grpc (ServerConfig "localhost" 50051 methods) $ \server ->
    forever $ do
      let method = head (registeredMethods server)
      result <- serverHandleNormalCall server method serverMeta $
        \_call reqBody _reqMeta -> return (reqBody, serverMeta, StatusOk,
                                           StatusDetails "")
      case result of
        Left x -> putStrLn $ "registered call result error: " ++ show x
        Right _ -> return ()

-- | loop to fork n times
regLoop :: Server -> RegisteredMethod -> IO ()
regLoop server method = forever $ do
  result <- serverHandleNormalCall server method serverMeta $
    \_call reqBody _reqMeta -> return (reqBody, serverMeta, StatusOk,
                                       StatusDetails "")
  case result of
    Left x -> putStrLn $ "registered call result error: " ++ show x
    Right _ -> return ()

regMainThreaded :: IO ()
regMainThreaded = do
  withGRPC $ \grpc -> do
    let methods = [(MethodName "/echo.Echo/DoEcho", Normal)]
    withServer grpc (ServerConfig "localhost" 50051 methods) $ \server -> do
      let method = head (registeredMethods server)
      tid1 <- async $ regLoop server method
      tid2 <- async $ regLoop server method
      wait tid1
      wait tid2
      return ()

main :: IO ()
main = regMainThreaded
