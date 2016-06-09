{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module LowLevelTests (lowLevelTests) where

import           Control.Concurrent                        (threadDelay)
import           Control.Concurrent.Async
import           Control.Monad
import           Data.ByteString                           (ByteString)
import qualified Data.Map                                  as M
import           Network.GRPC.LowLevel
import qualified Network.GRPC.LowLevel.Client.Unregistered as U
import qualified Network.GRPC.LowLevel.Server.Unregistered as U
import           Test.Tasty
import           Test.Tasty.HUnit                          as HU (Assertion,
                                                                  assertEqual,
                                                                  assertFailure,
                                                                  testCase,
                                                                  (@?=))

lowLevelTests :: TestTree
lowLevelTests = testGroup "Unit tests of low-level Haskell library"
  [ testGRPCBracket
  , testCompletionQueueCreateDestroy
  , testClientCreateDestroy
  , testClientCall
  , testClientTimeoutNoServer
  , testServerCreateDestroy
  , testServerCall
  , testServerTimeoutNoClient
  -- , testWrongEndpoint
  , testPayload
  , testPayloadUnregistered
  ]

testGRPCBracket :: TestTree
testGRPCBracket =
  testCase "Start/stop GRPC" $ withGRPC nop

testCompletionQueueCreateDestroy :: TestTree
testCompletionQueueCreateDestroy =
  testCase "Create/destroy CQ" $ withGRPC $ \g ->
  withCompletionQueue g nop

testClientCreateDestroy :: TestTree
testClientCreateDestroy =
  clientOnlyTest "start/stop" nop

testClientCall :: TestTree
testClientCall =
  clientOnlyTest "create/destroy call" $ \c -> do
    r <- U.withClientCall c "/foo" 10 $ const $ return $ Right ()
    r @?= Right ()

testClientTimeoutNoServer :: TestTree
testClientTimeoutNoServer =
  clientOnlyTest "request timeout when server DNE" $ \c -> do
    rm <- clientRegisterMethod c "/foo" Normal
    r  <- clientRequest c rm 1 "Hello" mempty
    r @?= Left GRPCIOUnknownError

testServerCreateDestroy :: TestTree
testServerCreateDestroy =
  serverOnlyTest "start/stop" [] nop

testServerCall :: TestTree
testServerCall =
  serverOnlyTest "create/destroy call" [] $ \s -> do
    r <- U.withServerCall s 1 $ const $ return $ Right ()
    r @?= Left GRPCIOTimeout

testServerTimeoutNoClient :: TestTree
testServerTimeoutNoClient =
  serverOnlyTest "wait timeout when client DNE" [("/foo", Normal)] $ \s -> do
    let rm = head (registeredMethods s)
    r <- serverHandleNormalCall s rm 1 mempty $ \_ _ ->
           return ("", mempty, mempty, StatusDetails "details")
    r @?= Left GRPCIOTimeout

-- TODO: fix this test: currently, client seems to hang and server times out,
-- expecting that the client reports an invalid endpoint.  Also, investigate
-- intermittent crashes on shorter server timeouts (tried 2, 5 seconds)
testWrongEndpoint :: TestTree
testWrongEndpoint =
  csTest "client requests unknown endpoint" client server [("/foo", Normal)]
  where
    -- TODO: possibly factor out dead-simple client/server construction even
    -- further
    client c = do
      rm <- clientRegisterMethod c "/bar" Normal
      r  <- clientRequest c rm 1 "Hello!" mempty
      r @?= Left (GRPCIOBadStatusCode GrpcStatusDeadlineExceeded
                    (StatusDetails "Deadline Exceeded"))
    server s = do
      length (registeredMethods s) @?= 1
      let rm = head (registeredMethods s)
      r <- serverHandleNormalCall s rm 10 mempty $ \_ _ -> do
        return ("reply test", dummyMeta, dummyMeta, StatusDetails "details string")
      r @?= Right ()

-- TODO: There seems to be a race here (and in other client/server pairs, of
-- course) about what gets reported when there is a failure. E.g., if one of the
-- Assertions fails in the request processing block for the server, we /may/ get
-- that error reported accurately as a call cancellation on the client, rather
-- than a useful error about the failure on the server. Maybe we'll need to
-- tweak EH behavior / async use.
testPayload :: TestTree
testPayload =
  csTest "registered normal request/response" client server [("/foo", Normal)]
  where
    clientMD = [("foo_key", "foo_val"), ("bar_key", "bar_val")]
    client c = do
      rm <- clientRegisterMethod c "/foo" Normal
      clientRequest c rm 10 "Hello!" clientMD >>= do
        checkReqRslt $ \NormalRequestResult{..} -> do
          rspCode @?= GrpcStatusOk
          rspBody @?= "reply test"
          details @?= "details string"
          initMD  @?= Just dummyMeta
          trailMD @?= dummyMeta
    server s = do
      length (registeredMethods s) @?= 1
      let rm = head (registeredMethods s)
      r <- serverHandleNormalCall s rm 11 mempty $ \reqBody reqMD -> do
        reqBody @?= "Hello!"
        checkMD "Server metadata mismatch" clientMD reqMD
        return ("reply test", dummyMeta, dummyMeta, StatusDetails "details string")
      r @?= Right ()

testPayloadUnregistered :: TestTree
testPayloadUnregistered =
  csTest "unregistered normal request/response" client server []
  where
    client c = do
      U.clientRequest c "/foo" 10 "Hello!" mempty >>= do
        checkReqRslt $ \NormalRequestResult{..} -> do
          rspCode @?= GrpcStatusOk
          rspBody @?= "reply test"
          details @?= "details string"
    server s = do
      r <- U.serverHandleNormalCall s 11 mempty $ \body _md meth -> do
             body @?= "Hello!"
             meth @?= "/foo"
             return ("reply test", mempty, "details string")
      r @?= Right ()

--------------------------------------------------------------------------------
-- Utilities and helpers

dummyMeta :: M.Map ByteString ByteString
dummyMeta = [("foo","bar")]

nop :: Monad m => a -> m ()
nop = const (return ())

serverOnlyTest :: TestName
               -> [(MethodName, GRPCMethodType)]
               -> (Server -> IO ())
               -> TestTree
serverOnlyTest nm ms =
  testCase ("Server - " ++ nm) . runTestServer . stdTestServer ms

clientOnlyTest :: TestName -> (Client -> IO ()) -> TestTree
clientOnlyTest nm =
  testCase ("Client - " ++ nm) . runTestClient . stdTestClient

csTest :: TestName
       -> (Client -> IO ())
       -> (Server -> IO ())
       -> [(MethodName, GRPCMethodType)]
       -> TestTree
csTest nm c s ms = csTest' nm (stdTestClient c) (stdTestServer ms s)

csTest' :: TestName -> TestClient -> TestServer -> TestTree
csTest' nm tc ts =
  testCase ("Client/Server - " ++ nm)
  $ void (s `concurrently` c)
  where
    -- We use a small delay to give the server a head start
    c = threadDelay 100000 >> runTestClient tc
    s = runTestServer ts

-- | @checkMD msg expected actual@ fails when keys from @expected@ are not in
-- @actual@, or when values differ for matching keys.
checkMD :: String -> MetadataMap -> MetadataMap -> Assertion
checkMD desc expected actual = do
  when (not $ M.null $ expected `diff` actual) $ do
    assertEqual desc expected (actual `M.intersection` expected)
  where
    diff = M.differenceWith $ \a b -> if a == b then Nothing else Just b

checkReqRslt :: Show a => (b -> Assertion) -> Either a b -> Assertion
checkReqRslt = either clientFail

clientFail :: Show a => a -> Assertion
clientFail = assertFailure . ("Client error: " ++). show

data TestClient = TestClient ClientConfig (Client -> IO ())

runTestClient :: TestClient -> IO ()
runTestClient (TestClient conf c) = withGRPC $ \g -> withClient g conf c

stdTestClient :: (Client -> IO ()) -> TestClient
stdTestClient = TestClient stdClientConf

stdClientConf :: ClientConfig
stdClientConf = ClientConfig "localhost" 50051

data TestServer = TestServer ServerConfig (Server -> IO ())

runTestServer :: TestServer -> IO ()
runTestServer (TestServer conf s) = withGRPC $ \g -> withServer g conf s

stdTestServer :: [(MethodName, GRPCMethodType)] -> (Server -> IO ()) -> TestServer
stdTestServer = TestServer . stdServerConf

stdServerConf :: [(MethodName, GRPCMethodType)] -> ServerConfig
stdServerConf = ServerConfig "localhost" 50051
