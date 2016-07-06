{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TupleSections     #-}
{-# LANGUAGE ViewPatterns      #-}

module LowLevelTests where

import           Control.Concurrent                        (threadDelay)
import           Control.Concurrent.Async
import           Control.Monad
import           Control.Monad.Managed
import           Data.ByteString                           (ByteString,
                                                            isPrefixOf,
                                                            isSuffixOf)
import qualified Data.Map                                  as M
import           Network.GRPC.LowLevel
import qualified Network.GRPC.LowLevel.Call.Unregistered   as U
import qualified Network.GRPC.LowLevel.Client.Unregistered as U
import qualified Network.GRPC.LowLevel.Server.Unregistered as U
import           Pipes                                     ((>->))
import qualified Pipes                                     as P
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
  , testMixRegisteredUnregistered
  , testPayload
  , testPayloadUnregistered
  , testServerCancel
  , testGoaway
  , testSlowServer
  , testServerCallExpirationCheck
  , testCustomUserAgent
  , testClientCompression
  , testClientServerCompression
  , testClientStreaming
  , testServerStreaming
  , testBiDiStreaming
  ]

testGRPCBracket :: TestTree
testGRPCBracket =
  testCase "Start/stop GRPC" $ runManaged $ void mgdGRPC

testCompletionQueueCreateDestroy :: TestTree
testCompletionQueueCreateDestroy =
  testCase "Create/destroy CQ" $ runManaged $ do
    g <- mgdGRPC
    liftIO (withCompletionQueue g nop)

testClientCreateDestroy :: TestTree
testClientCreateDestroy =
  clientOnlyTest "start/stop" nop

testClientTimeoutNoServer :: TestTree
testClientTimeoutNoServer =
  clientOnlyTest "request timeout when server DNE" $ \c -> do
    rm <- clientRegisterMethod c "/foo" Normal
    r  <- clientRequest c rm 1 "Hello" mempty
    r @?= Left GRPCIOTimeout

testServerCreateDestroy :: TestTree
testServerCreateDestroy =
  serverOnlyTest "start/stop" [] nop

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
          initMD @?= dummyMeta
          trailMD @?= dummyMeta
      clientRequest c rm2 1 "bad endpoint" mempty >>= do
        checkReqRslt $ \NormalRequestResult{..} -> do
          rspBody @?= ""
      return ()
    server s = do
       concurrently regThread unregThread
       return ()
       where regThread = do
               let rm = head (normalMethods s)
               r <- serverHandleNormalCall s rm dummyMeta $ \_ body _ -> do
                 body @?= "Hello"
                 return ("reply test", dummyMeta, StatusOk, "")
               return ()
             unregThread = do
               U.serverHandleNormalCall s mempty $ \call _ -> do
                 U.callMethod call @?= "/bar"
                 return ("", mempty, StatusOk,
                         StatusDetails "Wrong endpoint")
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
          initMD  @?= dummyMeta
          trailMD @?= dummyMeta
    server s = do
      let rm = head (normalMethods s)
      r <- serverHandleNormalCall s rm dummyMeta $ \_ reqBody reqMD -> do
        reqBody @?= "Hello!"
        checkMD "Server metadata mismatch" clientMD reqMD
        return ("reply test", dummyMeta, StatusOk, "details string")
      r @?= Right ()

testServerCancel :: TestTree
testServerCancel =
  csTest "server cancel call" client server [("/foo", Normal)]
  where
    client c = do
      rm <- clientRegisterMethod c "/foo" Normal
      res <- clientRequest c rm 10 "" mempty
      res @?= badStatus StatusCancelled
    server s = do
      let rm = head (normalMethods s)
      r <- serverHandleNormalCall s rm mempty $ \c _ _ -> do
        serverCallCancel c StatusCancelled ""
        return (mempty, mempty, StatusCancelled, "")
      r @?= Right ()

testServerStreaming :: TestTree
testServerStreaming =
  csTest "server streaming" client server [("/feed", ServerStreaming)]
  where
    clientInitMD = [("client","initmd")]
    serverInitMD = [("server","initmd")]
    clientPay    = "FEED ME!"
    pays         = ["ONE", "TWO", "THREE", "FOUR"] :: [ByteString]

    client c = do
      rm <- clientRegisterMethod c "/feed" ServerStreaming
      eea <- clientReader c rm 10 clientPay clientInitMD $ \initMD recv -> do
        liftIO $ checkMD "Server initial metadata mismatch" serverInitMD initMD
        forM_ pays $ \p -> recv `is` Right (Just p)
        recv `is` Right Nothing
      eea @?= Right (dummyMeta, StatusOk, "dtls")

    server s = do
      let rm = head (sstreamingMethods s)
      eea <- serverWriter s rm serverInitMD $ \sc send -> do
        liftIO $ do
          checkMD "Client request metadata mismatch"
            clientInitMD (requestMetadataRecv sc)
          case optionalPayload sc of
            Nothing  -> assertFailure "expected optional payload"
            Just pay -> pay @?= clientPay
        forM_ pays $ \p -> send p `is` Right ()
        return (dummyMeta, StatusOk, "dtls")
      eea @?= Right ()

testClientStreaming :: TestTree
testClientStreaming =
  csTest "client streaming" client server [("/slurp", ClientStreaming)]
  where
    clientInitMD = [("a","b")]
    serverInitMD = [("x","y")]
    trailMD      = dummyMeta
    serverRsp    = "serverReader reply"
    serverDtls   = "deets"
    serverStatus = StatusOk
    pays         = ["P_ONE", "P_TWO", "P_THREE"] :: [ByteString]

    client c = do
      rm  <- clientRegisterMethod c "/slurp" ClientStreaming
      eea <- clientWriter c rm 10 clientInitMD $ \send -> do
        -- liftIO $ checkMD "Server initial metadata mismatch" serverInitMD initMD
        forM_ pays $ \p -> send p `is` Right ()
      eea @?= Right (Just serverRsp, serverInitMD, trailMD, serverStatus, serverDtls)

    server s = do
      let rm = head (cstreamingMethods s)
      eea <- serverReader s rm serverInitMD $ \sc recv -> do
        liftIO $ checkMD "Client request metadata mismatch"
                   clientInitMD (requestMetadataRecv sc)
        forM_ pays $ \p -> recv `is` Right (Just p)
        recv `is` Right Nothing
        return (Just serverRsp, trailMD, serverStatus, serverDtls)
      eea @?= Right ()

testBiDiStreaming :: TestTree
testBiDiStreaming =
  csTest "bidirectional streaming" client server [("/bidi", BiDiStreaming)]
  where
    clientInitMD = [("bidi-streaming","client")]
    serverInitMD = [("bidi-streaming","server")]
    trailMD      = dummyMeta
    serverStatus = StatusOk
    serverDtls   = "deets"

    client c = do
      rm  <- clientRegisterMethod c "/bidi" BiDiStreaming
      eea <- clientRW c rm 10 clientInitMD $ \initMD recv send -> do
        liftIO $ checkMD "Server initial metadata mismatch"
                   serverInitMD initMD
        send "cw0" `is` Right ()
        recv       `is` Right (Just "sw0")
        send "cw1" `is` Right ()
        recv       `is` Right (Just "sw1")
        recv       `is` Right (Just "sw2")
        return ()
      eea @?= Right (trailMD, serverStatus, serverDtls)

    server s = do
      let rm = head (bidiStreamingMethods s)
      eea <- serverRW s rm serverInitMD $ \sc recv send -> do
        liftIO $ checkMD "Client request metadata mismatch"
                   clientInitMD (requestMetadataRecv sc)
        recv       `is` Right (Just "cw0")
        send "sw0" `is` Right ()
        recv       `is` Right (Just "cw1")
        send "sw1" `is` Right ()
        send "sw2" `is` Right ()
        recv       `is` Right Nothing
        return (trailMD, serverStatus, serverDtls)
      eea @?= Right ()

--------------------------------------------------------------------------------
-- Unregistered tests

testClientCall :: TestTree
testClientCall =
  clientOnlyTest "create/destroy call" $ \c -> do
    r <- U.withClientCall c "/foo" 10 $ const $ return $ Right ()
    r @?= Right ()

testServerCall :: TestTree
testServerCall =
  serverOnlyTest "create/destroy call" [] $ \s -> do
    r <- U.withServerCall s $ const $ return $ Right ()
    r @?= Left GRPCIOTimeout

testPayloadUnregistered :: TestTree
testPayloadUnregistered =
  csTest "unregistered normal request/response" client server []
  where
    client c =
      U.clientRequest c "/foo" 10 "Hello!" mempty >>= do
        checkReqRslt $ \NormalRequestResult{..} -> do
          rspCode @?= StatusOk
          rspBody @?= "reply test"
          details @?= "details string"
    server s = do
      r <- U.serverHandleNormalCall s mempty $ \U.ServerCall{..} body -> do
             body @?= "Hello!"
             callMethod @?= "/foo"
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
        lastResult == badStatus StatusUnavailable
        ||
        lastResult == badStatus StatusDeadlineExceeded
        ||
        lastResult == Left GRPCIOTimeout
    server s = do
      let rm = head (normalMethods s)
      serverHandleNormalCall s rm mempty dummyHandler
      serverHandleNormalCall s rm mempty dummyHandler
      return ()

testSlowServer :: TestTree
testSlowServer =
  csTest "Client handles slow server response" client server [("/foo", Normal)]
  where
    client c = do
      rm <- clientRegisterMethod c "/foo" Normal
      result <- clientRequest c rm 1 "" mempty
      result @?= badStatus StatusDeadlineExceeded
    server s = do
      let rm = head (normalMethods s)
      serverHandleNormalCall s rm mempty $ \_ _ _ -> do
        threadDelay (2*10^(6 :: Int))
        return dummyResp
      return ()

testServerCallExpirationCheck :: TestTree
testServerCallExpirationCheck =
  csTest "Check for call expiration" client server [("/foo", Normal)]
  where
    client c = do
      rm <- clientRegisterMethod c "/foo" Normal
      result <- clientRequest c rm 3 "" mempty
      return ()
    server s = do
      let rm = head (normalMethods s)
      serverHandleNormalCall s rm mempty $ \c _ _ -> do
        exp1 <- serverCallIsExpired c
        assertBool "Call isn't expired when handler starts" $ not exp1
        threadDelaySecs 1
        exp2 <- serverCallIsExpired c
        assertBool "Call isn't expired after 1 second" $ not exp2
        threadDelaySecs 3
        exp3 <- serverCallIsExpired c
        assertBool "Call is expired after 4 seconds" exp3
        return dummyResp
      return ()

testCustomUserAgent :: TestTree
testCustomUserAgent =
  csTest' "Server sees custom user agent prefix/suffix" client server
  where
    clientArgs = [UserAgentPrefix "prefix!", UserAgentSuffix "suffix!"]
    client =
      TestClient (ClientConfig "localhost" 50051 clientArgs) $
        \c -> do rm <- clientRegisterMethod c "/foo" Normal
                 result <- clientRequest c rm 4 "" mempty
                 return ()
    server = TestServer (stdServerConf [("/foo", Normal)]) $ \s -> do
      let rm = head (normalMethods s)
      serverHandleNormalCall s rm mempty $ \_ _ meta -> do
        let ua = meta M.! "user-agent"
        assertBool "User agent prefix is present" $ isPrefixOf "prefix!" ua
        assertBool "User agent suffix is present" $ isSuffixOf "suffix!" ua
        return dummyResp
      return ()

testClientCompression :: TestTree
testClientCompression =
  csTest' "client-only compression: no errors" client server
  where
    client =
      TestClient (ClientConfig
                   "localhost"
                   50051
                   [CompressionAlgArg GrpcCompressDeflate]) $ \c -> do
        rm <- clientRegisterMethod c "/foo" Normal
        result <- clientRequest c rm 1 "hello" mempty
        return ()
    server = TestServer (stdServerConf [("/foo", Normal)]) $ \s -> do
      let rm = head (normalMethods s)
      serverHandleNormalCall s rm mempty $ \_ body _ -> do
        body @?= "hello"
        return dummyResp
      return ()

testClientServerCompression :: TestTree
testClientServerCompression =
  csTest' "client/server compression: no errors" client server
  where
    cconf = ClientConfig "localhost"
                         50051
                         [CompressionAlgArg GrpcCompressDeflate]
    client = TestClient cconf $ \c -> do
      rm <- clientRegisterMethod c "/foo" Normal
      clientRequest c rm 1 "hello" mempty >>= do
        checkReqRslt $ \NormalRequestResult{..} -> do
          rspCode @?= StatusOk
          rspBody @?= "hello"
          details @?= ""
          initMD  @?= dummyMeta
          trailMD @?= dummyMeta
      return ()
    sconf = ServerConfig "localhost"
                         50051
                         [("/foo", Normal)]
                         [CompressionAlgArg GrpcCompressDeflate]
    server = TestServer sconf $ \s -> do
      let rm = head (normalMethods s)
      serverHandleNormalCall s rm dummyMeta $ \_sc body _ -> do
        body @?= "hello"
        return ("hello", dummyMeta, StatusOk, StatusDetails "")
      return ()

--------------------------------------------------------------------------------
-- Utilities and helpers

is :: (Eq a, Show a, MonadIO m) => m a -> a -> m ()
is act x = act >>= liftIO . (@?= x)

dummyMeta :: M.Map ByteString ByteString
dummyMeta = [("foo","bar")]

dummyResp :: (ByteString, MetadataMap, StatusCode, StatusDetails)
dummyResp = ("", mempty, StatusOk, StatusDetails "")

dummyHandler :: ServerCall -> ByteString -> MetadataMap
                -> IO (ByteString, MetadataMap, StatusCode, StatusDetails)
dummyHandler _ _ _ = return dummyResp

dummyResult' :: StatusDetails
             -> IO (ByteString, MetadataMap, StatusCode, StatusDetails)
dummyResult' = return . (mempty, mempty, StatusOk, )

badStatus :: StatusCode -> Either GRPCIOError a
badStatus st = Left . GRPCIOBadStatusCode st $ case st of
  StatusDeadlineExceeded -> "Deadline Exceeded"
  StatusCancelled        -> "Received RST_STREAM err=8"
  _ -> mempty

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
checkMD desc expected actual =
  unless (M.null $ expected `diff` actual) $
    assertEqual desc expected (actual `M.intersection` expected)
  where
    diff = M.differenceWith $ \a b -> if a == b then Nothing else Just b

checkReqRslt :: Show a => (b -> Assertion) -> Either a b -> Assertion
checkReqRslt = either clientFail

-- | The consumer which asserts that the next value it consumes is equal to the
-- given value; string parameter used as in 'assertEqual'.
assertConsumeEq :: (Eq a, Show a) => String -> a -> P.Consumer a IO ()
assertConsumeEq s v = P.lift . assertEqual s v =<< P.await

clientFail :: Show a => a -> Assertion
clientFail = assertFailure . ("Client error: " ++). show

data TestClient = TestClient ClientConfig (Client -> IO ())

runTestClient :: TestClient -> IO ()
runTestClient (TestClient conf f) =
  runManaged $ mgdGRPC >>= mgdClient conf >>= liftIO . f

stdTestClient :: (Client -> IO ()) -> TestClient
stdTestClient = TestClient stdClientConf

stdClientConf :: ClientConfig
stdClientConf = ClientConfig "localhost" 50051 []

data TestServer = TestServer ServerConfig (Server -> IO ())

runTestServer :: TestServer -> IO ()
runTestServer (TestServer conf f) =
  runManaged $ mgdGRPC >>= mgdServer conf >>= liftIO . f

stdTestServer :: [(MethodName, GRPCMethodType)] -> (Server -> IO ()) -> TestServer
stdTestServer = TestServer . stdServerConf

stdServerConf :: [(MethodName, GRPCMethodType)] -> ServerConfig
stdServerConf xs = ServerConfig "localhost" 50051 xs []

threadDelaySecs :: Int -> IO ()
threadDelaySecs = threadDelay . (* 10^(6::Int))

mgdGRPC :: Managed GRPC
mgdGRPC = managed withGRPC

mgdClient :: ClientConfig -> GRPC -> Managed Client
mgdClient conf g = managed $ withClient g conf

mgdServer :: ServerConfig -> GRPC -> Managed Server
mgdServer conf g = managed $ withServer g conf
