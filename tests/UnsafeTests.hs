{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module UnsafeTests (unsafeTests, unsafeProperties) where

import           Control.Concurrent                        (threadDelay)
import           Control.Exception (bracket_)
import           Control.Monad
import qualified Data.ByteString                as B
import           Foreign.Marshal.Alloc
import           Foreign.Storable
import           GHC.Exts
import           Network.GRPC.LowLevel.GRPC (threadDelaySecs)
import           Network.GRPC.Unsafe
import           Network.GRPC.Unsafe.ByteBuffer
import           Network.GRPC.Unsafe.Metadata
import           Network.GRPC.Unsafe.Slice
import           Network.GRPC.Unsafe.Time
import           Network.GRPC.Unsafe.ChannelArgs
import           System.Clock
import           Test.Tasty
import           Test.Tasty.HUnit               as HU (testCase, (@?=),
                                                       assertBool)
import           Test.Tasty.QuickCheck          as QC
import           Test.Tasty.HUnit               as HU (testCase, (@?=))

unsafeTests :: TestTree
unsafeTests = testGroup "Unit tests for unsafe C bindings"
  [ roundtripSlice "Hello, world!"
  , roundtripSlice "\NULabc\NUL"
  , roundtripByteBuffer "Hwaet! We gardena in geardagum..."
  , roundtripSlice largeByteString
  , roundtripByteBuffer largeByteString
  , roundtripTimeSpec (TimeSpec 123 123)
  , testMetadata
  , testNow
  , testCreateDestroyMetadata
  , testCreateDestroyMetadataKeyVals
  , testCreateDestroyDeadline
  , testCreateDestroyChannelArgs
  ]

unsafeProperties :: TestTree
unsafeProperties = testGroup "QuickCheck properties for unsafe C bindings"
  [ metadataIsList ]

instance Arbitrary B.ByteString where
  arbitrary = B.pack <$> arbitrary

instance Arbitrary MetadataMap where
  arbitrary = fromList <$> arbitrary

metadataIsList :: TestTree
metadataIsList = QC.testProperty "Metadata IsList instance" $
                   \(md :: MetadataMap) -> md == (fromList $ toList md)

largeByteString :: B.ByteString
largeByteString = B.pack $ take (32*1024*1024) $ cycle [97..99]

roundtripSlice :: B.ByteString -> TestTree
roundtripSlice bs = testCase "ByteString slice roundtrip" $ do
  slice <- byteStringToSlice bs
  unslice <- sliceToByteString slice
  unslice HU.@?= bs
  freeSlice slice

roundtripByteBuffer :: B.ByteString -> TestTree
roundtripByteBuffer bs = testCase "ByteBuffer roundtrip" $ do
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

roundtripTimeSpec :: TimeSpec -> TestTree
roundtripTimeSpec t = testCase "CTimeSpec roundtrip" $ do
  p <- malloc
  let c = CTimeSpec t
  poke p c
  c' <- peek p
  c' @?= c
  free p

testMetadata :: TestTree
testMetadata = testCase "Metadata setter/getter roundtrip" $ do
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
testNow = testCase "Create/destroy various clock types" $ do
  _ <- currTimeMillis GprClockMonotonic
  _ <- currTimeMillis GprClockRealtime
  _ <- currTimeMillis GprClockPrecise
  return ()

testCreateDestroyMetadata :: TestTree
testCreateDestroyMetadata = testCase "Create/destroy metadataArrayPtr" $ do
  grpc $ withMetadataArrayPtr $ const $ return ()

testCreateDestroyMetadataKeyVals :: TestTree
testCreateDestroyMetadataKeyVals = testCase "Create/destroy metadata key/values" $ do
  grpc $ withMetadataKeyValPtr 10 $ const $ return ()

testCreateDestroyDeadline :: TestTree
testCreateDestroyDeadline = testCase "Create/destroy deadline" $ do
  grpc $ withDeadlineSeconds 10 $ const $ return ()

testCreateDestroyChannelArgs :: TestTree
testCreateDestroyChannelArgs = testCase "Create/destroy channel args" $
  grpc $ withChannelArgs [CompressionAlgArg GrpcCompressDeflate] $
  const $ return ()

assertCqEventComplete :: Event -> IO ()
assertCqEventComplete e = do
  eventCompletionType e HU.@?= OpComplete
  eventSuccess e HU.@?= True

grpc :: IO a -> IO ()
grpc = bracket_ grpcInit grpcShutdown . void

_nowarnUnused :: a
_nowarnUnused = assertCqEventComplete `undefined` threadDelaySecs
