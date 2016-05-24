{-# LANGUAGE OverloadedStrings #-}

module UnsafeTests where

import           Data.ByteString                as B (ByteString)
import           Network.GRPC.Unsafe
import           Network.GRPC.Unsafe.ByteBuffer
import           Network.GRPC.Unsafe.Metadata
import           Network.GRPC.Unsafe.Slice
import           Network.GRPC.Unsafe.Time
import           Test.Tasty
import           Test.Tasty.HUnit               as HU (testCase, (@?=))

unsafeTests :: TestTree
unsafeTests = testGroup "Unit tests for unsafe C bindings."
  [ roundtripSlice "Hello, world!"
  , roundtripByteBuffer "Hwaet! We gardena in geardagum..."
  , testMetadata
  , testNow
  , testCreateDestroyMetadata
  , testCreateDestroyMetadataKeyVals
  , testCreateDestroyDeadline
  ]

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
  -- clean up
  freeSlice slice
  byteBufferReaderDestroy reader
  grpcByteBufferDestroy buffer
  freeSlice readSlice

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

testCreateDestroyMetadata :: TestTree
testCreateDestroyMetadata = testCase "create/destroy metadataArrayPtr " $ do
  grpcInit
  withMetadataArrayPtr $ const $ return ()
  grpcShutdown

testCreateDestroyMetadataKeyVals :: TestTree
testCreateDestroyMetadataKeyVals = testCase "create/destroy metadata k/vs " $ do
  grpcInit
  withMetadataKeyValPtr 10 $ const $ return ()
  grpcShutdown

testCreateDestroyDeadline :: TestTree
testCreateDestroyDeadline = testCase "create/destroy deadline " $ do
  grpcInit
  withDeadlineSeconds 10 $ const $ return ()
  grpcShutdown
