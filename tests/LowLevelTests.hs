{-# LANGUAGE OverloadedStrings #-}

module LowLevelTests (lowLevelTests) where

import           Control.Concurrent.Async
import           Control.Monad
import           Data.ByteString                (ByteString)
import qualified Data.Map                       as M
import           Network.GRPC.LowLevel
import           Test.Tasty
import           Test.Tasty.HUnit               as HU (testCase, (@?=))

lowLevelTests :: TestTree
lowLevelTests = testGroup "Unit tests of low-level Haskell library"
  [  testGRPCBracket
   , testCompletionQueueCreateDestroy
   , testServerCreateDestroy
   , testClientCreateDestroy
   , testWithServerCall
   , testWithClientCall
   , testPayloadLowLevel
   , testClientRequestNoServer
   , testServerAwaitNoClient
   , testPayloadLowLevelUnregistered
   ]

testGRPCBracket :: TestTree
testGRPCBracket = grpcTest "Start/stop GRPC" nop

testCompletionQueueCreateDestroy :: TestTree
testCompletionQueueCreateDestroy =
  grpcTest "Create/destroy completion queue" $ \grpc -> do
  withCompletionQueue grpc nop

testServerCreateDestroy :: TestTree
testServerCreateDestroy =
  grpcTest "Server - start/stop" $ \grpc -> do
  withServer grpc (ServerConfig "localhost" 50051 []) nop

testClientCreateDestroy :: TestTree
testClientCreateDestroy =
  grpcTest "Client - start/stop" $ \grpc -> do
  withClient grpc (ClientConfig "localhost" 50051) nop

payloadLowLevelServer :: TestServer
payloadLowLevelServer = TestServer $ \grpc -> do
  let conf = (ServerConfig "localhost" 50051 [("/foo", Normal)])
  withServer grpc conf $ \server -> do
    let method = head (registeredMethods server)
    result <- serverHandleNormalRegisteredCall server method 11 M.empty $
                \reqBody reqMeta -> do
                  reqMeta M.! "foo_key" @?= "foo_val"
                  reqBody @?= "Hello!"
                  return ("reply test", dummyMeta, dummyMeta,
                          StatusDetails "details string")
    case result of
      Left err -> error $ show err
      Right _ -> return ()

payloadLowLevelClient :: TestClient
payloadLowLevelClient = TestClient $ \grpc ->
  withClient grpc (ClientConfig "localhost" 50051) $ \client -> do
    method <- clientRegisterMethod client "/foo" "localhost" Normal
    putStrLn "registered method on client."
    let reqMeta = M.fromList [("foo_key", "foo_val")]
    reqResult <- clientRegisteredRequest client method 10 "Hello!" reqMeta
    case reqResult of
      Left x -> error $ "Client got error: " ++ show x
      Right (NormalRequestResult respBody (Just initMeta) trailingMeta respCode details) -> do
        details @?= "details string"
        respBody @?= "reply test"
        respCode @?= GrpcStatusOk
        initMeta M.! "foo" @?= "bar"
        trailingMeta M.! "foo" @?= "bar"
      Right (NormalRequestResult _ Nothing _ _ _) -> error $ "got no metadata."

payloadLowLevelClientUnregistered :: TestClient
payloadLowLevelClientUnregistered = TestClient $ \grpc -> do
  withClient grpc (ClientConfig "localhost" 50051) $ \client -> do
    reqResult <- clientRequest client "/foo" "localhost" 10 "Hello!" M.empty
    case reqResult of
      Left x -> error $ "Client got error: " ++ show x
      Right (NormalRequestResult
              respBody _initMeta _trailingMeta respCode details) -> do
        respBody @?= "reply test"
        respCode @?= GrpcStatusOk
        details @?= "details string"

payloadLowLevelServerUnregistered :: TestServer
payloadLowLevelServerUnregistered = TestServer $ \grpc -> do
  withServer grpc (ServerConfig "localhost" 50051 []) $ \server -> do
    result <- serverHandleNormalCall server 11 M.empty $
                \reqBody _reqMeta reqMethod -> do
                  reqBody @?= "Hello!"
                  reqMethod @?= "/foo"
                  return ("reply test", M.empty, StatusDetails "details string")
    case result of
      Left x -> error $ show x
      Right _ -> return ()

testClientRequestNoServer :: TestTree
testClientRequestNoServer =
  grpcTest "Client - request timeout when server DNE" $ \grpc -> do
  withClient grpc (ClientConfig "localhost" 50051) $ \client -> do
    method <- clientRegisterMethod client "/foo" "localhost" Normal
    reqResult <- clientRegisteredRequest client method 1 "Hello" M.empty
    reqResult @?= (Left GRPCIOTimeout)

testServerAwaitNoClient :: TestTree
testServerAwaitNoClient = testCase "server wait times out when no client " $ do
  withGRPC $ \grpc -> do
    let conf = (ServerConfig "localhost" 50051 [("/foo", Normal)])
    withServer grpc conf $ \server -> do
      let method = head (registeredMethods server)
      result <- serverHandleNormalRegisteredCall server method 1 M.empty $
                  \_ _ -> return ("", M.empty, M.empty, StatusDetails "details")
      result @?= Left GRPCIOTimeout

testServerUnregisteredAwaitNoClient :: TestTree
testServerUnregisteredAwaitNoClient =
  testCase "server wait times out when no client -- unregistered method " $ do
    withGRPC $ \grpc -> do
      let conf = ServerConfig "localhost" 50051 []
      withServer grpc conf $ \server -> do
        result <- serverHandleNormalCall server 10 M.empty $
                    \_ _ _ -> return ("", M.empty, StatusDetails "")
        case result of
          Left err -> error $ show err
          Right _ -> return ()

testPayloadLowLevel :: TestTree
testPayloadLowLevel =
  grpcTest "Client/Server - low-level (registered) request/response" $
  runClientServer payloadLowLevelClient payloadLowLevelServer

testPayloadLowLevelUnregistered :: TestTree
testPayloadLowLevelUnregistered =
  grpcTest "Client/Server - low-level unregistered request/response" $
  runClientServer payloadLowLevelClientUnregistered payloadLowLevelServerUnregistered

testWithServerCall :: TestTree
testWithServerCall =
  grpcTest "Server - Create/destroy call" $ \grpc -> do
  let conf = ServerConfig "localhost" 50051 []
  withServer grpc conf $ \server -> do
    result <- withServerUnregCall server 1 $ const $ return $ Right ()
    result @?= Left GRPCIOTimeout

testWithClientCall :: TestTree
testWithClientCall =
  grpcTest "Client - Create/destroy call" $ \grpc -> do
  let conf = ClientConfig "localhost" 50051
  withClient grpc conf $ \client -> do
    result <- withClientCall client "foo" "localhost" 10 $
                const $ return $ Right ()
    case result of
      Left err -> error $ show err
      Right _ -> return ()

--------------------------------------------------------------------------------
-- Utilities and helpers

dummyMeta :: M.Map ByteString ByteString
dummyMeta = M.fromList [("foo","bar")]

nop :: Monad m => a -> m ()
nop = const (return ())

-- | Defines a general-purpose GRPC unit test
grpcTest :: TestName -> (GRPC -> IO ()) -> TestTree
grpcTest nm = testCase nm . withGRPC

newtype TestClient = TestClient (GRPC -> IO ())
newtype TestServer = TestServer (GRPC -> IO ())

-- | Concurrently executes the given 'TestClient' and 'TestServer' TODO: We may
-- want to add toplevel timeouts and better error reporting here.
runClientServer :: TestClient -> TestServer -> GRPC -> IO ()
runClientServer (TestClient c) (TestServer s) grpc =
  void $ s grpc `concurrently` c grpc
