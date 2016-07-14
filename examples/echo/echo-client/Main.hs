{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# OPTIONS_GHC -fno-warn-unused-binds       #-}

import           Control.Monad
import qualified Data.ByteString.Lazy                      as BL
import           Data.Protobuf.Wire.Class
import           Data.Protobuf.Wire.Types
import qualified Data.Text                                 as T
import           Data.Word
import           GHC.Generics                              (Generic)
import           Network.GRPC.LowLevel
import qualified Network.GRPC.LowLevel.Client.Unregistered as U
import           Proto3.Wire.Decode                        (ParseError)

echoMethod = MethodName "/echo.Echo/DoEcho"
addMethod = MethodName "/echo.Add/DoAdd"

_unregistered c = U.clientRequest c echoMethod 1 "hi" mempty

regMain = withGRPC $ \g ->
  withClient g (ClientConfig "localhost" 50051 []) $ \c -> do
  rm <- clientRegisterMethodNormal c echoMethod
  replicateM_ 100000 $ clientRequest c rm 5 "hi" mempty >>= \case
    Left e -> error $ "Got client error: " ++ show e
    Right r
      | rspBody r == "hi" -> return ()
      | otherwise -> error $ "Got unexpected payload: " ++ show r

-- NB: If you change these, make sure to change them in the server as well.
-- TODO: Put these in a common location (or just hack around it until CG is working)
data EchoRequest = EchoRequest {message :: T.Text} deriving (Show, Eq, Ord, Generic)
instance Message EchoRequest
data AddRequest = AddRequest {addX :: Fixed Word32, addY :: Fixed Word32} deriving (Show, Eq, Ord, Generic)
instance Message AddRequest
data AddResponse = AddResponse {answer :: Fixed Word32} deriving (Show, Eq, Ord, Generic)
instance Message AddResponse

-- TODO: Create Network.GRPC.HighLevel.Client w/ request variants

highlevelMain = withGRPC $ \g ->
    withClient g (ClientConfig "localhost" 50051 []) $ \c -> do
    rm <- clientRegisterMethodNormal c echoMethod
    let pay = EchoRequest "hi"
        enc = BL.toStrict . toLazyByteString $ pay
    replicateM_ 1 $ clientRequest c rm 5 enc mempty >>= \case
      Left e  -> error $ "Got client error: " ++ show e
      Right r -> case fromByteString (rspBody r) of
        Left e -> error $ "Got decoding error: " ++ show e
        Right dec
          | dec == pay -> return ()
          | otherwise -> error $ "Got unexpected payload: " ++ show dec
    rmAdd <- clientRegisterMethodNormal c addMethod
    let addPay = AddRequest 1 2
        addEnc = BL.toStrict . toLazyByteString $ addPay
    replicateM_ 1 $ clientRequest c rmAdd 5 addEnc mempty >>= \case
      Left e -> error $ "Got client error on add request: " ++ show e
      Right r -> case fromByteString (rspBody r) of
        Left e -> error $ "failed to decode add response: " ++ show e
        Right dec
          | dec == AddResponse 3 -> return ()
          | otherwise -> error $ "Got wrong add answer: " ++ show dec

main = highlevelMain
