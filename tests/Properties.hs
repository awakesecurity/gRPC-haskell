{-# LANGUAGE OverloadedStrings #-}

import           Control.Concurrent.Async
import qualified Data.ByteString                as B
import           Foreign.Marshal.Alloc
import           Foreign.Ptr
import           Foreign.Storable
import           Network.GRPC.Unsafe
import           Network.GRPC.Unsafe.ByteBuffer
import           Network.GRPC.Unsafe.Constants
import           Network.GRPC.Unsafe.Metadata
import           Network.GRPC.Unsafe.Op
import           Network.GRPC.Unsafe.Slice
import           Network.GRPC.Unsafe.Time
import           Test.Tasty
import           Test.Tasty.HUnit               as HU

import           LowLevelTests

roundtripSlice :: B.ByteString -> TestTree
roundtripSlice bs = testCase "Slice C bindings roundtrip" $ do
  slice <- byteStringToSlice bs
  unslice <- sliceToByteString slice
  bs HU.@?= unslice
  freeSlice slice

roundtripByteBuffer :: B.ByteString -> TestTree
roundtripByteBuffer bs = testCase "ByteBuffer C bindings roundtrip" $ do
  slice <- byteStringToSlice bs
  buffer <- grpcRawByteBufferCreate slice 1
  reader <- byteBufferReaderCreate buffer
  readSlice <- grpcByteBufferReaderReadall reader
  bs' <- sliceToByteString readSlice
  bs' HU.@?= bs
  --clean up
  freeSlice slice
  byteBufferReaderDestroy reader
  grpcByteBufferDestroy buffer
  freeSlice readSlice

currTimeMillis :: ClockType -> IO Int
currTimeMillis t = do
  gprT <- gprNow t
  tMillis <- gprTimeToMillis gprT
  timespecDestroy gprT
  return tMillis

testNow :: TestTree
testNow = testCase "create and destroy various clock types" $ do
  _ <- currTimeMillis GprClockMonotonic
  _ <- currTimeMillis GprClockRealtime
  _ <- currTimeMillis GprClockPrecise
  return ()

testMetadata :: TestTree
testMetadata = testCase "metadata setter/getter C bindings roundtrip" $ do
  m <- metadataAlloc 3
  setMetadataKeyVal "hello" "world" m 0
  setMetadataKeyVal "foo" "bar" m 1
  setMetadataKeyVal "Haskell" "Curry" m 2
  k0 <- getMetadataKey m 0
  v0 <- getMetadataVal m 0
  k1 <- getMetadataKey m 1
  v1 <- getMetadataVal m 1
  k2 <- getMetadataKey m 2
  v2 <- getMetadataVal m 2
  k0 HU.@?= "hello"
  v0 HU.@?= "world"
  k1 HU.@?= "foo"
  v1 HU.@?= "bar"
  k2 HU.@?= "Haskell"
  v2 HU.@?= "Curry"
  metadataFree m

assertCqEventComplete :: Event -> IO ()
assertCqEventComplete e = do
  eventCompletionType e HU.@?= OpComplete
  eventSuccess e HU.@?= True

testPayloadClient :: IO ()
testPayloadClient = do
  client <- grpcInsecureChannelCreate "localhost:50051" nullPtr reserved
  cq <- grpcCompletionQueueCreate reserved
  withMetadataArrayPtr $ \initialMetadataRecv -> do
    withMetadataArrayPtr $ \trailingMetadataRecv -> do
      withByteBufferPtr $ \clientRecvBB -> do
        deadline <- secondsToDeadline 5
        pluckDeadline <- secondsToDeadline 10
        clientCall <- grpcChannelCreateCall
                        client (Call nullPtr) propagateDefaults cq
                        "/foo" "localhost" deadline reserved
        --send request
        withOpArray 6 $ \ops -> do
          opSendInitialMetadataEmpty ops 0
          withByteStringAsByteBuffer "hello world" $ \requestPayload -> do
            opSendMessage ops 1 requestPayload
            opSendCloseClient ops 2
            opRecvInitialMetadata ops 3 initialMetadataRecv
            opRecvMessage ops 4 clientRecvBB
            statusCodePtr <- createStatusCodePtr
            let cstringCapacity = 32
            cStringPtr <- malloc
            cstring <- mallocBytes cstringCapacity
            poke cStringPtr cstring
            opRecvStatusClient ops 5 trailingMetadataRecv statusCodePtr
                               cStringPtr
                               cstringCapacity
            --send client request
            requestError <- grpcCallStartBatch clientCall ops 6 (tag 1) reserved
            clientRequestCqEvent <- grpcCompletionQueuePluck
                                      cq (tag 1) pluckDeadline reserved
            assertCqEventComplete clientRequestCqEvent
            requestError HU.@?= CallOk
            free cstring
            free cStringPtr
            destroyStatusCodePtr statusCodePtr
        --verify response received
        responseRecv <- peek clientRecvBB
        responseRecvBS <- copyByteBufferToByteString responseRecv
        responseRecvBS HU.@?= "hello you"
        grpcCompletionQueueShutdown cq
        grpcCallDestroy clientCall
        --TODO: the grpc test drains the cq here
        grpcCompletionQueueDestroy cq
        grpcChannelDestroy client

testPayloadServer :: IO ()
testPayloadServer = do
  server <- grpcServerCreate nullPtr reserved
  cq <- grpcCompletionQueueCreate reserved
  grpcServerRegisterCompletionQueue server cq reserved
  _ <- grpcServerAddInsecureHttp2Port server "localhost:50051"
  grpcServerStart server
  serverCallPtr <- malloc
  withMetadataArrayPtr $ \requestMetadataRecv -> do
    withByteBufferPtr $ \recvBufferPtr -> do
      callDetails <- createCallDetails
      requestMetadataRecv' <- peek requestMetadataRecv
      recvRequestError <- grpcServerRequestCall
                            server serverCallPtr callDetails
                            requestMetadataRecv' cq cq (tag 101)
      pluckDeadline' <- secondsToDeadline 10
      requestCallCqEvent <- grpcCompletionQueuePluck cq (tag 101)
                                                     pluckDeadline'
                                                     reserved
      assertCqEventComplete requestCallCqEvent
      recvRequestError HU.@?= CallOk
      destroyCallDetails callDetails
      --receive request
      withOpArray 2 $ \recvOps -> do
        opSendInitialMetadataEmpty recvOps 0
        opRecvMessage recvOps 1 recvBufferPtr
        serverCall <- peek serverCallPtr
        recvBatchError <- grpcCallStartBatch serverCall recvOps 2
                                             (tag 102) reserved
        recvBatchError HU.@?= CallOk
        pluckDeadline'' <- secondsToDeadline 10
        recvCqEvent <- grpcCompletionQueuePluck cq (tag 102)
                                                pluckDeadline''
                                                reserved
        assertCqEventComplete recvCqEvent
      --send response
      withOpArray 3 $ \respOps -> do
        withByteStringAsByteBuffer "hello you" $ \respbb -> do
          cancelledPtr <- malloc
          opRecvCloseServer respOps 0 cancelledPtr
          opSendMessage respOps 1 respbb
          opSendStatusServer respOps 2 0 (MetadataKeyValPtr nullPtr)
                             GrpcStatusOk "ok"
          serverCall <- peek serverCallPtr
          respBatchError <- grpcCallStartBatch serverCall respOps 3
                                               (tag 103) reserved
          respBatchError HU.@?= CallOk
          pluckDeadline''' <- secondsToDeadline 10
          respCqEvent <- grpcCompletionQueuePluck cq (tag 103)
                                                  pluckDeadline'''
                                                  reserved
          assertCqEventComplete respCqEvent
      --verify data was received
      serverRecv <- peek recvBufferPtr
      serverRecvBS <- copyByteBufferToByteString serverRecv
      serverRecvBS HU.@?= "hello world"
      --shut down
      grpcServerShutdownAndNotify server cq (tag 0)
      pluckDeadline'''' <- secondsToDeadline 10
      shutdownEvent <- grpcCompletionQueuePluck cq (tag 0) pluckDeadline''''
                                                reserved
      assertCqEventComplete shutdownEvent
      grpcServerCancelAllCalls server
      grpcServerDestroy server
      grpcCompletionQueueShutdown cq
      grpcCompletionQueueDestroy cq
      free serverCallPtr

-- | Straightforward translation of the gRPC core test end2end/tests/payload.c
-- This is intended to test the low-level C bindings, so we use only a few
-- minimal abstractions on top of it.
testPayload :: TestTree
testPayload = testCase "low-level C bindings request/response " $ do
  grpcInit
  withAsync testPayloadServer $ \a1 -> do
    withAsync testPayloadClient $ \a2 -> do
      () <- wait a1
      wait a2
  grpcShutdown
  putStrLn "Done."

testCreateDestroyMetadata :: TestTree
testCreateDestroyMetadata = testCase "create/destroy metadataArrayPtr " $ do
  grpcInit
  withMetadataArrayPtr $ const (return ())
  grpcShutdown

testCreateDestroyMetadataKeyVals :: TestTree
testCreateDestroyMetadataKeyVals = testCase "create/destroy metadata k/vs " $ do
  grpcInit
  withMetadataKeyValPtr 10 $ const (return ())
  grpcShutdown

testCreateDestroyDeadline :: TestTree
testCreateDestroyDeadline = testCase "create/destroy deadline " $ do
  grpcInit
  withDeadlineSeconds 10 $ const (return ())
  grpcShutdown

unsafeTests :: TestTree
unsafeTests = testGroup "Unit tests for unsafe C bindings."
  [testPayload,
   roundtripSlice "Hello, world!",
   roundtripByteBuffer "Hwaet! We gardena in geardagum...",
   testMetadata,
   testNow,
   testCreateDestroyMetadata,
   testCreateDestroyMetadataKeyVals,
   testCreateDestroyDeadline
   ]

allTests :: TestTree
allTests = testGroup "All tests"
  [ unsafeTests,
   lowLevelTests]

main :: IO ()
main = defaultMain allTests
