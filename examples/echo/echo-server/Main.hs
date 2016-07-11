{-# LANGUAGE DataKinds                       #-}
{-# LANGUAGE OverloadedLists                 #-}
{-# LANGUAGE OverloadedStrings               #-}
{-# LANGUAGE RecordWildCards                 #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# OPTIONS_GHC -fno-warn-unused-binds       #-}

import           Control.Concurrent
import           Control.Concurrent.Async
import           Control.Monad
import           Data.ByteString                           (ByteString)
import           Network.GRPC.LowLevel
import           Network.GRPC.LowLevel.Call
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
  withServer grpc (ServerConfig "localhost" 50051 [] []) $ \server -> forever $ do
    result <- U.serverHandleNormalCall server serverMeta handler
    case result of
      Left x -> putStrLn $ "handle call result error: " ++ show x
      Right _ -> return ()

regMain :: IO ()
regMain = withGRPC $ \grpc -> do
  let methods = [(MethodName "/echo.Echo/DoEcho", Normal)]
  withServer grpc (ServerConfig "localhost" 50051 methods []) $ \server ->
    forever $ do
      let method = head (normalMethods server)
      result <- serverHandleNormalCall server method serverMeta $
        \_call reqBody _reqMeta -> return (reqBody, serverMeta, StatusOk,
                                           StatusDetails "")
      case result of
        Left x -> putStrLn $ "registered call result error: " ++ show x
        Right _ -> return ()

tputStrLn x = do
  tid <- myThreadId
  putStrLn $ "[" ++ show tid ++ "]: " ++ x

regLoop :: Server -> RegisteredMethod 'Normal -> IO ()
regLoop server method = forever $ do
--  tputStrLn "about to block on call handler"
  result <- serverHandleNormalCall server method serverMeta $
    \_call reqBody _reqMeta ->
      return (reqBody, serverMeta, StatusOk, StatusDetails "")
  case result of
    Left x -> error $! "registered call result error: " ++ show x
    Right _ -> return ()

regMainThreaded :: IO ()
regMainThreaded = do
  withGRPC $ \grpc -> do
    let methods = [(MethodName "/echo.Echo/DoEcho", Normal)]
    withServer grpc (ServerConfig "localhost" 50051 methods []) $ \server -> do
      let method = head (normalMethods server)
      tids <- replicateM 7 $ async $ do tputStrLn "starting handler"
                                        regLoop server method
      waitAnyCancel tids
      tputStrLn "finishing"

main :: IO ()
main = regMainThreaded
