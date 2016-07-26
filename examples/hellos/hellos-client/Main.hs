{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns        #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# OPTIONS_GHC -fno-warn-unused-binds       #-}

import           Control.Monad
import qualified Data.ByteString.Lazy                      as BL
import           Data.Protobuf.Wire.Class
import qualified Data.Text                                 as T
import           Data.Word
import           GHC.Generics                              (Generic)
import           Network.GRPC.LowLevel

helloSS = MethodName "/hellos.Hellos/HelloSS"
helloCS = MethodName "/hellos.Hellos/HelloCS"

data SSRqt = SSRqt { ssName :: T.Text, ssNumReplies :: Word32 } deriving (Show, Eq, Ord, Generic)
instance Message SSRqt
data SSRpy = SSRpy { ssGreeting :: T.Text } deriving (Show, Eq, Ord, Generic)
instance Message SSRpy
data CSRqt = CSRqt { csMessage :: T.Text } deriving (Show, Eq, Ord, Generic)
instance Message CSRqt
data CSRpy = CSRpy { csNumRequests :: Word32 } deriving (Show, Eq, Ord, Generic)
instance Message CSRpy

expect :: (Eq a, Monad m, Show a) => String -> a -> a -> m ()
expect ctx ex got
  | ex /= got = fail $ ctx ++ " error: expected " ++ show ex ++ ", got " ++ show got
  | otherwise = return ()

doHelloSS c = do
  rm <- clientRegisterMethodServerStreaming c helloSS
  let nr  = 10
      pay = SSRqt "server streaming mode" nr
      enc = BL.toStrict . toLazyByteString $ pay
  eea <- clientReader c rm 5 enc mempty $ \_md recv -> do
    n :: Int <- go recv 0
    expect "doHelloSS/cnt" (fromIntegral nr) n
  case eea of
    Left e             -> fail $ "clientReader error: " ++ show e
    Right (_, st, _)
      | st /= StatusOk -> fail "clientReader: non-OK status"
      | otherwise      -> return ()
  where
    expay     = "Hello there, server streaming mode!"
    go recv n = recv >>= \case
      Left e         -> fail $ "doHelloSS error: " ++ show e
      Right Nothing  -> return n
      Right (Just r) -> case fromByteString r of
        Left e   -> fail $ "Decoding error: " ++ show e
        Right r' -> do
          expect "doHelloSS/rpy" expay (ssGreeting r')
          go recv (n+1)

doHelloCS c = do
  rm  <- clientRegisterMethodClientStreaming c helloCS
  let nr = 10
      pay = CSRqt "client streaming payload"
      enc = BL.toStrict . toLazyByteString $ pay
  eea <- clientWriter c rm 10 mempty $ \send ->
    replicateM_ (fromIntegral nr) $ send enc >>= \case
      Left e  -> fail $ "doHelloCS: send error: " ++ show e
      Right{} -> return ()
  case eea of
    Left e                      -> fail $ "clientWriter error: " ++ show e
    Right (Nothing, _, _, _, _) -> fail "clientWriter error: no reply payload"
    Right (Just bs, _init, _trail, st, _dtls)
      | st /= StatusOk -> fail "clientWriter: non-OK status"
      | otherwise -> case fromByteString bs of
          Left e    -> fail $ "Decoding error: " ++ show e
          Right dec -> expect "doHelloCS/cnt" nr (csNumRequests dec)

highlevelMain = withGRPC $ \g ->
  withClient g (ClientConfig "localhost" 50051 []) $ \c -> do
    doHelloSS c
    doHelloCS c

main = highlevelMain
