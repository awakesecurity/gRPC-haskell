{-# LANGUAGE OverloadedStrings #-}

module LowLevelTests where

import           Control.Concurrent.Async (withAsync, wait)
import           Data.ByteString (ByteString)
import qualified Data.Map as M
import           Network.GRPC.LowLevel
import           Test.Tasty
import           Test.Tasty.HUnit ((@?=), testCase)

lowLevelTests :: TestTree
lowLevelTests = testGroup "Unit tests of low-level Haskell library"
  [ testGRPCBracket
  , testCompletionQueueCreateDestroy
  , testServerCreateDestroy
  , testClientCreateDestroy
  , testWithServerCall
  , testWithClientCall
  -- , testPayloadLowLevel --TODO: currently crashing from free on unalloced ptr
  -- , testClientRequestNoServer --TODO: succeeds when no other tests run.
  , testServerAwaitNoClient
  -- , testPayloadLowLevelUnregistered --TODO: succeeds when no other tests run.
  ]

dummyMeta :: M.Map ByteString ByteString
dummyMeta = M.fromList [("foo","bar")]

testGRPCBracket :: TestTree
testGRPCBracket = testCase "No errors starting and stopping GRPC" $
                  withGRPC $ const $ return ()

testCompletionQueueCreateDestroy :: TestTree
testCompletionQueueCreateDestroy =
  testCase "No errors creating and destroying a CQ" $ withGRPC $ \grpc ->
    withCompletionQueue grpc $ const (return ())

testServerCreateDestroy :: TestTree
testServerCreateDestroy =
  testCase "No errors when starting and stopping a server" $
  withGRPC $ \grpc -> withServer grpc (ServerConfig "localhost" 50051 [])
                                 (const $ return ())

testClientCreateDestroy :: TestTree
testClientCreateDestroy =
  testCase "No errors when starting and stopping a client" $
  withGRPC $ \grpc -> withClient grpc (ClientConfig "localhost" 50051)
                                 (const $ return ())

testPayloadLowLevelServer :: GRPC -> IO ()
testPayloadLowLevelServer grpc = do
  let conf = (ServerConfig "localhost" 50051 [("/foo", "localhost", Normal)])
  withServer grpc conf $ \server -> do
    let method = head (registeredMethods server)
    result <- serverHandleNormalRegisteredCall server method 11 M.empty $
                \reqBody reqMeta -> return ("reply test", dummyMeta, dummyMeta)
    case result of
      Left err -> error $ show err
      Right _ -> return ()

testPayloadLowLevelClient :: GRPC -> IO ()
testPayloadLowLevelClient grpc =
  withClient grpc (ClientConfig "localhost" 50051) $ \client -> do
    method <- clientRegisterMethod client "/foo" "localhost" Normal
    putStrLn "registered method on client."
    reqResult <- clientRegisteredRequest client method 10 "Hello!" M.empty
    case reqResult of
      Left x -> error $ "Client got error: " ++ show x
      Right (NormalRequestResult respBody initMeta trailingMeta respCode) -> do
        respBody @?= "reply test"
        respCode @?= GrpcStatusOk

testPayloadLowLevelClientUnregistered :: GRPC -> IO ()
testPayloadLowLevelClientUnregistered grpc = do
  withClient grpc (ClientConfig "localhost" 50051) $ \client -> do
    reqResult <- clientRequest client "/foo" "localhost" 10 "Hello!" M.empty
    case reqResult of
      Left x -> error $ "Client got error: " ++ show x
      Right (NormalRequestResult respBody initMeta trailingMeta respCode) -> do
        respBody @?= "reply test"
        respCode @?= GrpcStatusOk

testPayloadLowLevelServerUnregistered :: GRPC -> IO ()
testPayloadLowLevelServerUnregistered grpc = do
  withServer grpc (ServerConfig "localhost" 50051 []) $ \server -> do
    result <- serverHandleNormalCall server 11 M.empty $
                \reqBody reqMeta -> return ("reply test", M.empty)
    case result of
      Left x -> error $ show x
      Right _ -> return ()

testClientRequestNoServer :: TestTree
testClientRequestNoServer = testCase "request times out when no server " $ do
  withGRPC $ \grpc -> do
    withClient grpc (ClientConfig "localhost" 50051) $ \client -> do
      method <- clientRegisterMethod client "/foo" "localhost" Normal
      reqResult <- clientRegisteredRequest client method 1 "Hello" M.empty
      reqResult @?= (Left GRPCIOTimeout)

testServerAwaitNoClient :: TestTree
testServerAwaitNoClient = testCase "server wait times out when no client " $ do
  withGRPC $ \grpc -> do
    let conf = (ServerConfig "localhost" 50051 [("/foo", "localhost", Normal)])
    withServer grpc conf $ \server -> do
      let method = head (registeredMethods server)
      result <- serverHandleNormalRegisteredCall server method 1 M.empty $
                  \_ _ -> return ("", M.empty, M.empty)
      result @?= Left GRPCIOTimeout

testServerUnregisteredAwaitNoClient :: TestTree
testServerUnregisteredAwaitNoClient =
  testCase "server wait times out when no client -- unregistered method " $ do
    withGRPC $ \grpc -> do
      let conf = ServerConfig "localhost" 50051 []
      withServer grpc conf $ \server -> do
        result <- serverHandleNormalCall server 10 M.empty $
                    \_ _ -> return ("", M.empty)
        case result of
          Left err -> error $ show err
          Right _ -> return ()

testPayloadLowLevel :: TestTree
testPayloadLowLevel = testCase "LowLevel Haskell library request/response " $ do
  withGRPC $ \grpc -> do
    withAsync (testPayloadLowLevelServer grpc) $ \a1 -> do
      withAsync (testPayloadLowLevelClient grpc) $ \a2 -> do
        wait a1
        wait a2

testPayloadLowLevelUnregistered :: TestTree
testPayloadLowLevelUnregistered =
  testCase "LowLevel Haskell library unregistered request/response " $ do
    withGRPC $ \grpc -> do
      withAsync (testPayloadLowLevelServerUnregistered grpc) $ \a1 ->
        withAsync (testPayloadLowLevelClientUnregistered grpc) $ \a2 -> do
          wait a1
          wait a2

testWithServerCall :: TestTree
testWithServerCall =
  testCase "Creating and destroying a call: no errors. " $
    withGRPC $ \grpc -> do
      let conf = ServerConfig "localhost" 50051 []
      withServer grpc conf $ \server -> do
        result <- withServerCall server 1 $ const $ return $ Right ()
        result @?= Left GRPCIOTimeout

testWithClientCall :: TestTree
testWithClientCall =
  testCase "Creating and destroying a client call: no errors. " $
    withGRPC $ \grpc -> do
      let conf = ClientConfig "localhost" 50051
      withClient grpc conf $ \client -> do
        result <- withClientCall client "foo" "localhost" 10 $
                    const $ return $ Right ()
        case result of
          Left err -> error $ show err
          Right _ -> return ()
