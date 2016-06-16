{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module LowLevelTests where

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
                                                                  assertBool,
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
  , testWrongEndpoint
  , testMixRegisteredUnregistered
  , testPayload
  , testPayloadUnregistered
  , testServerCancel
  , testGoaway
  , testSlowServer
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
    r <- serverHandleNormalCall s rm 1 mempty dummyHandler
    r @?= Left GRPCIOTimeout

testWrongEndpoint :: TestTree
testWrongEndpoint =
  csTest "client requests unknown endpoint" client server [("/foo", Normal)]
  where
    -- TODO: possibly factor out dead-simple client/server construction even
    -- further
    client c = do
      rm <- clientRegisterMethod c "/bar" Normal
      r  <- clientRequest c rm 1 "Hello!" mempty
      r @?= Left (GRPCIOBadStatusCode StatusDeadlineExceeded
                    (StatusDetails "Deadline Exceeded"))
    server s = do
      length (registeredMethods s) @?= 1
      let rm = head (registeredMethods s)
      r <- serverHandleNormalCall s rm 2 mempty dummyHandler
      r @?= Left GRPCIOTimeout

testMixRegisteredUnregistered :: TestTree
testMixRegisteredUnregistered =
  csTest "server uses unregistered calls to handle unknown endpoints"
         client
         server
         [("/foo", Normal)]
  where
    client c = do
      rm1 <- clientRegisterMethod c "/foo" Normal
      rm2 <- clientRegisterMethod c "/bar" Normal
      clientRequest c rm1 1 "Hello" mempty >>= do
        checkReqRslt $ \NormalRequestResult{..} -> do
          rspBody @?= "reply test"
          initMD @?= Just dummyMeta
          trailMD @?= dummyMeta
      clientRequest c rm2 1 "bad endpoint" mempty >>= do
        checkReqRslt $ \NormalRequestResult{..} -> do
          rspBody @?= ""
      r3 <- clientRequest c rm1 1 "Hello" mempty
      r3 @?= deadlineExceededStatus
      return ()
    server s = do
       concurrently regThread unregThread
       return ()
       where regThread = do
               let rm = head (registeredMethods s)
               r <- serverHandleNormalCall s rm 2 dummyMeta $ \_ body _ -> do
                 body @?= "Hello"
                 return ("reply test", dummyMeta, StatusOk, StatusDetails "")
               return ()
             unregThread = do
               r1 <- U.serverHandleNormalCall s 2 mempty $ \_ _ _ method -> do
                 method @?= "/bar"
                 return ("", mempty, StatusOk,
                         StatusDetails "Wrong endpoint")
               r2 <- U.serverHandleNormalCall s 2 mempty $ \_ _ _ method -> do
                 method @?= "/bar"
                 return ("", mempty, StatusNotFound,
                         StatusDetails "Wrong endpoint")
               r2 @?= Left GRPCIOTimeout
               return ()

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
          rspCode @?= StatusOk
          rspBody @?= "reply test"
          details @?= "details string"
          initMD  @?= Just dummyMeta
          trailMD @?= dummyMeta
    server s = do
      length (registeredMethods s) @?= 1
      let rm = head (registeredMethods s)
      r <- serverHandleNormalCall s rm 11 dummyMeta $ \_ reqBody reqMD -> do
        reqBody @?= "Hello!"
        checkMD "Server metadata mismatch" clientMD reqMD
        return ("reply test", dummyMeta, StatusOk,
                StatusDetails "details string")
      r @?= Right ()

testServerCancel :: TestTree
testServerCancel =
  csTest "server cancel call" client server [("/foo", Normal)]
  where
    client c = do
      rm <- clientRegisterMethod c "/foo" Normal
      res <- clientRequest c rm 10 "" mempty
      res @?= Left (GRPCIOBadStatusCode StatusCancelled
                                        (StatusDetails
                                          "Received RST_STREAM err=8"))
    server s = do
      let rm = head (registeredMethods s)
      r <- serverHandleNormalCall s rm 10 mempty $ \c _ _ -> do
        serverCallCancel c StatusCancelled ""
        return (mempty, mempty, StatusOk, "")
      r @?= Right ()

testPayloadUnregistered :: TestTree
testPayloadUnregistered =
  csTest "unregistered normal request/response" client server []
  where
    client c = do
      U.clientRequest c "/foo" 10 "Hello!" mempty >>= do
        checkReqRslt $ \NormalRequestResult{..} -> do
          rspCode @?= StatusOk
          rspBody @?= "reply test"
          details @?= "details string"
    server s = do
      r <- U.serverHandleNormalCall s 11 mempty $ \_ body _md meth -> do
             body @?= "Hello!"
             meth @?= "/foo"
             return ("reply test", mempty, StatusOk, "details string")
      r @?= Right ()

testGoaway :: TestTree
testGoaway =
  csTest "Client handles server shutdown gracefully"
         client
         server
         [("/foo", Normal)]
  where
    client c = do
      rm <- clientRegisterMethod c "/foo" Normal
      clientRequest c rm 10 "" mempty
      clientRequest c rm 10 "" mempty
      lastResult <- clientRequest c rm 1 "" mempty
      assertBool "Client handles server shutdown gracefully" $
        lastResult == unavailableStatus
        ||
        lastResult == deadlineExceededStatus
    server s = do
      let rm = head (registeredMethods s)
      serverHandleNormalCall s rm 11 mempty dummyHandler
      serverHandleNormalCall s rm 11 mempty dummyHandler
      return ()

testSlowServer :: TestTree
testSlowServer =
  csTest "Client handles slow server response" client server [("/foo", Normal)]
  where
    client c = do
      rm <- clientRegisterMethod c "/foo" Normal
      result <- clientRequest c rm 1 "" mempty
      result @?= deadlineExceededStatus
    server s = do
      let rm = head (registeredMethods s)
      serverHandleNormalCall s rm 1 mempty $ \_ _ _ -> do
        threadDelay (2*10^(6 :: Int))
        return ("", mempty, StatusOk, StatusDetails "")
      return ()

--------------------------------------------------------------------------------
-- Utilities and helpers

dummyMeta :: M.Map ByteString ByteString
dummyMeta = [("foo","bar")]

dummyHandler :: ServerCall -> ByteString -> MetadataMap
                -> IO (ByteString, MetadataMap, StatusCode, StatusDetails)
dummyHandler _ _ _ = return ("", mempty, StatusOk, StatusDetails "")

unavailableStatus :: Either GRPCIOError a
unavailableStatus =
  Left (GRPCIOBadStatusCode StatusUnavailable (StatusDetails ""))

deadlineExceededStatus :: Either GRPCIOError a
deadlineExceededStatus =
  Left (GRPCIOBadStatusCode StatusDeadlineExceeded
                            (StatusDetails "Deadline Exceeded"))

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
