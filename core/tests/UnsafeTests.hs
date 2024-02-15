{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module UnsafeTests (unsafeTests, unsafeProperties) where

import Control.Exception (bracket_)
import Control.Monad
import qualified Data.ByteString as B
import Data.List.NonEmpty (NonEmpty ((:|)))
import qualified Data.Map as M
import Foreign.Marshal.Alloc
import Foreign.Storable
import GHC.Exts
import Network.GRPC.LowLevel.GRPC (MetadataMap (..), threadDelaySecs)
import qualified Network.GRPC.LowLevel.GRPC.MetadataMap as MD
import Network.GRPC.Unsafe
import Network.GRPC.Unsafe.ByteBuffer
import Network.GRPC.Unsafe.ChannelArgs
import Network.GRPC.Unsafe.Metadata
import Network.GRPC.Unsafe.Security
import Network.GRPC.Unsafe.Slice
import Network.GRPC.Unsafe.Time
import System.Clock
import Test.QuickCheck.Gen
import Test.Tasty
import Test.Tasty.HUnit as HU (Assertion, testCase, (@?=))
import Test.Tasty.QuickCheck as QC

unsafeTests :: TestTree
unsafeTests =
  testGroup
    "Unit tests for unsafe C bindings"
    [ roundtripSliceUnit "\NULabc\NUL"
    , roundtripSliceUnit largeByteString
    , roundtripByteBufferUnit largeByteString
    , roundtripTimeSpec (TimeSpec 123 123)
    , testMetadata
    , testMetadataOrdering
    , testMetadataOrderingProp
    , testNow
    , testCreateDestroyMetadata
    , testCreateDestroyMetadataKeyVals
    , testCreateDestroyDeadline
    , testCreateDestroyChannelArgs
    , testCreateDestroyClientCreds
    , testCreateDestroyServerCreds
    ]

unsafeProperties :: TestTree
unsafeProperties =
  testGroup
    "QuickCheck properties for unsafe C bindings"
    [ roundtripSliceQC
    , roundtripByteBufferQC
    , roundtripMetadataQC
    , metadataIsList
    , roundtripMetadataOrdering
    ]

instance Arbitrary B.ByteString where
  arbitrary = B.pack <$> arbitrary

instance Arbitrary MetadataMap where
  arbitrary = do
    -- keys are not allowed to contain \NUL, but values are.
    let key = arbitrary `suchThat` B.notElem 0
    ks0 <- listOf key
    duplicateKeys <- arbitrary
    ks <-
      if duplicateKeys
        then (ks0 <>) . concat . replicate 2 <$> listOf1 key
        else pure ks0
    fromList . zip ks <$> vector (length ks)

roundtripMetadataKeyVals :: MetadataMap -> IO MetadataMap
roundtripMetadataKeyVals m = do
  (kvPtr, l) <- createMetadata m
  m' <- getAllMetadata kvPtr l
  metadataFree kvPtr
  return m'

roundtripMetadataQC :: TestTree
roundtripMetadataQC = QC.testProperty "Metadata roundtrip" $
  \m -> QC.ioProperty $ do
    m' <- roundtripMetadataKeyVals m
    return $ m === m'

metadataIsList :: TestTree
metadataIsList = QC.testProperty "Metadata IsList instance" $
  \(md :: MetadataMap) -> md == (fromList $ toList md)

roundtripMetadataOrdering :: TestTree
roundtripMetadataOrdering =
  QC.testProperty "Metadata map ordering" $
    QC.ioProperty . checkMetadataOrdering

largeByteString :: B.ByteString
largeByteString = B.pack $ take (32 * 1024 * 1024) $ cycle [97 .. 99]

roundtripSlice :: B.ByteString -> IO B.ByteString
roundtripSlice bs = do
  slice <- byteStringToSlice bs
  unslice <- sliceToByteString slice
  freeSlice slice
  return unslice

roundtripSliceQC :: TestTree
roundtripSliceQC = QC.testProperty "Slice roundtrip: QuickCheck" $
  \bs -> QC.ioProperty $ do
    bs' <- roundtripSlice bs
    return $ bs == bs'

roundtripSliceUnit :: B.ByteString -> TestTree
roundtripSliceUnit bs = testCase "ByteString slice roundtrip" $ do
  unslice <- roundtripSlice bs
  unslice @?= bs

roundtripByteBuffer :: B.ByteString -> IO B.ByteString
roundtripByteBuffer bs = do
  slice <- byteStringToSlice bs
  buffer <- grpcRawByteBufferCreate slice 1
  reader <- byteBufferReaderCreate buffer
  readSlice <- grpcByteBufferReaderReadall reader
  bs' <- sliceToByteString readSlice
  freeSlice slice
  byteBufferReaderDestroy reader
  grpcByteBufferDestroy buffer
  freeSlice readSlice
  return bs'

roundtripByteBufferQC :: TestTree
roundtripByteBufferQC = QC.testProperty "ByteBuffer roundtrip: QuickCheck" $
  \bs -> QC.ioProperty $ do
    bs' <- roundtripByteBuffer bs
    return $ bs == bs'

roundtripByteBufferUnit :: B.ByteString -> TestTree
roundtripByteBufferUnit bs = testCase "ByteBuffer roundtrip" $ do
  bs' <- roundtripByteBuffer bs
  bs' @?= bs

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
  k0 @?= "hello"
  v0 @?= "world"
  k1 @?= "foo"
  v1 @?= "bar"
  k2 @?= "Haskell"
  v2 @?= "Curry"
  metadataFree m

testMetadataOrdering :: TestTree
testMetadataOrdering = testCase "Metadata map ordering (simple)" $ do
  let m0 = fromList @MetadataMap [("foo", "bar"), ("fnord", "FNORD")]
  let m1 = fromList @MetadataMap [("foo", "baz")]
  let lr = m0 <> m1
  let rl = m1 <> m0
  M.lookup "foo" (unMap lr) @?= Just ["bar", "baz"]
  M.lookup "foo" (unMap rl) @?= Just ["baz", "bar"]
  toList lr @?= [("fnord", "FNORD"), ("foo", "bar"), ("foo", "baz")]
  toList rl @?= [("fnord", "FNORD"), ("foo", "baz"), ("foo", "bar")]
  M.lookup "foo" (unMap (lr <> rl)) @?= Just ["bar", "baz", "baz", "bar"]
  MD.lookupAll "foo" lr @?= Just ("bar" :| ["baz"])
  MD.lookupLast "foo" lr @?= Just "baz"
  MD.lookupAll "foo" rl @?= Just ("baz" :| ["bar"])
  MD.lookupLast "foo" rl @?= Just "bar"

testMetadataOrderingProp :: TestTree
testMetadataOrderingProp =
  testCase "Metadata map ordering prop w/ trivial inputs" $
    mapM_
      (checkMetadataOrdering . fromList)
      [ [("foo", "bar"), ("fnord", "FNORD"), ("foo", "baz")]
      , [("foo", "baz"), ("fnord", "FNORD"), ("foo", "bar")]
      ]

checkMetadataOrdering :: MetadataMap -> Assertion
checkMetadataOrdering md0 = do
  let ikvps = toList md0 `zip` [0 ..]
  let ok md = unMap md @?= M.unionsWith (<>) [M.singleton k [v] | ((k, v), _i) <- ikvps]
  ok md0
  md1 <- do
    let n = length ikvps
    withMetadataKeyValPtr n $ \m -> do
      let deref i = (,) <$> getMetadataKey m i <*> getMetadataVal m i
      mapM_ (\((k, v), i) -> setMetadataKeyVal k v m i) ikvps
      mapM_ (\(kvp, i) -> deref i >>= (@?= kvp)) ikvps
      getAllMetadata m n
  ok md1
  -- Check Eq instance
  mapM_ (uncurry (@?=)) [(x, y) | x <- [md0, md1], y <- [md0, md1]]

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
testCreateDestroyChannelArgs =
  testCase "Create/destroy channel args" $
    grpc $
      withChannelArgs [CompressionAlgArg GrpcCompressDeflate] $
        const $
          return ()

testCreateDestroyClientCreds :: TestTree
testCreateDestroyClientCreds =
  testCase "Create/destroy client credentials" $
    grpc $
      withChannelCredentials Nothing Nothing Nothing $
        const $
          return ()

testCreateDestroyServerCreds :: TestTree
testCreateDestroyServerCreds =
  testCase "Create/destroy server credentials"
    $ grpc
    $ withServerCredentials
      Nothing
      "tests/ssl/testServerKey.pem"
      "tests/ssl/testServerCert.pem"
      SslDontRequestClientCertificate
    $ const
    $ return ()

assertCqEventComplete :: Event -> IO ()
assertCqEventComplete e = do
  eventCompletionType e @?= OpComplete
  eventSuccess e @?= True

grpc :: IO a -> IO ()
grpc = bracket_ grpcInit grpcShutdownBlocking . void

_nowarnUnused :: a
_nowarnUnused = assertCqEventComplete `undefined` threadDelaySecs
