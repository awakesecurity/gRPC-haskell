{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
-- | Generated by Haskell protocol buffer compiler. DO NOT EDIT!
module Arithmetic where
import qualified Prelude as Hs
import qualified Proto3.Suite.DotProto as HsProtobuf
import qualified Proto3.Suite.Types as HsProtobuf
import qualified Proto3.Suite.Class as HsProtobuf
import qualified Proto3.Wire as HsProtobuf
import Control.Applicative ((<*>), (<|>))
import qualified Data.Text.Lazy as Hs (Text)
import qualified Data.ByteString as Hs
import qualified Data.String as Hs (fromString)
import qualified Data.Vector as Hs (Vector)
import qualified Data.Int as Hs (Int16, Int32, Int64)
import qualified Data.Word as Hs (Word16, Word32, Word64)
import GHC.Generics as Hs
import GHC.Enum as Hs
import Network.GRPC.HighLevel.Generated as HsGRPC
import Network.GRPC.HighLevel.Client as HsGRPC
import Network.GRPC.HighLevel.Server as HsGRPC hiding (serverLoop)
import Network.GRPC.HighLevel.Server.Unregistered as HsGRPC
       (serverLoop)
import Network.GRPC.LowLevel.Call as HsGRPC
 
data Arithmetic request response = Arithmetic{arithmeticAdd ::
                                              request 'HsGRPC.Normal TwoInts OneInt ->
                                                Hs.IO (response 'HsGRPC.Normal OneInt),
                                              arithmeticRunningSum ::
                                              request 'HsGRPC.ClientStreaming OneInt OneInt ->
                                                Hs.IO (response 'HsGRPC.ClientStreaming OneInt)}
                                 deriving Hs.Generic
 
arithmeticServer ::
                   Arithmetic HsGRPC.ServerRequest HsGRPC.ServerResponse ->
                     HsGRPC.ServiceOptions -> Hs.IO ()
arithmeticServer
  Arithmetic{arithmeticAdd = arithmeticAdd,
             arithmeticRunningSum = arithmeticRunningSum}
  (ServiceOptions serverHost serverPort useCompression
     userAgentPrefix userAgentSuffix initialMetadata sslConfig logger)
  = (HsGRPC.serverLoop
       HsGRPC.defaultOptions{HsGRPC.optNormalHandlers =
                               [(HsGRPC.UnaryHandler
                                   (HsGRPC.MethodName "/arithmetic.Arithmetic/Add")
                                   (HsGRPC.convertGeneratedServerHandler arithmeticAdd))],
                             HsGRPC.optClientStreamHandlers =
                               [(HsGRPC.ClientStreamHandler
                                   (HsGRPC.MethodName "/arithmetic.Arithmetic/RunningSum")
                                   (HsGRPC.convertGeneratedServerReaderHandler
                                      arithmeticRunningSum))],
                             HsGRPC.optServerStreamHandlers = [],
                             HsGRPC.optBiDiStreamHandlers = [], optServerHost = serverHost,
                             optServerPort = serverPort, optUseCompression = useCompression,
                             optUserAgentPrefix = userAgentPrefix,
                             optUserAgentSuffix = userAgentSuffix,
                             optInitialMetadata = initialMetadata, optSSLConfig = sslConfig,
                             optLogger = logger})
 
arithmeticClient ::
                   HsGRPC.Client ->
                     Hs.IO (Arithmetic HsGRPC.ClientRequest HsGRPC.ClientResult)
arithmeticClient client
  = (Hs.pure Arithmetic) <*>
      ((Hs.pure (HsGRPC.clientRequest client)) <*>
         (HsGRPC.clientRegisterMethod client
            (HsGRPC.MethodName "/arithmetic.Arithmetic/Add")))
      <*>
      ((Hs.pure (HsGRPC.clientRequest client)) <*>
         (HsGRPC.clientRegisterMethod client
            (HsGRPC.MethodName "/arithmetic.Arithmetic/RunningSum")))
 
data TwoInts = TwoInts{twoIntsX :: Hs.Int32, twoIntsY :: Hs.Int32}
             deriving (Hs.Show, Hs.Eq, Hs.Ord, Hs.Generic)
 
instance HsProtobuf.Named TwoInts where
        nameOf _ = (Hs.fromString "TwoInts")
 
instance HsProtobuf.Message TwoInts where
        encodeMessage _ TwoInts{twoIntsX = twoIntsX, twoIntsY = twoIntsY}
          = (Hs.mconcat
               [(HsProtobuf.encodeMessageField (HsProtobuf.FieldNumber 1)
                   twoIntsX),
                (HsProtobuf.encodeMessageField (HsProtobuf.FieldNumber 2)
                   twoIntsY)])
        decodeMessage _
          = (Hs.pure TwoInts) <*>
              (HsProtobuf.at HsProtobuf.decodeMessageField
                 (HsProtobuf.FieldNumber 1))
              <*>
              (HsProtobuf.at HsProtobuf.decodeMessageField
                 (HsProtobuf.FieldNumber 2))
        dotProto _
          = [(HsProtobuf.DotProtoField (HsProtobuf.FieldNumber 1)
                (HsProtobuf.Prim HsProtobuf.Int32)
                (HsProtobuf.Single "x")
                []
                Hs.Nothing),
             (HsProtobuf.DotProtoField (HsProtobuf.FieldNumber 2)
                (HsProtobuf.Prim HsProtobuf.Int32)
                (HsProtobuf.Single "y")
                []
                Hs.Nothing)]
 
data OneInt = OneInt{oneIntResult :: Hs.Int32}
            deriving (Hs.Show, Hs.Eq, Hs.Ord, Hs.Generic)
 
instance HsProtobuf.Named OneInt where
        nameOf _ = (Hs.fromString "OneInt")
 
instance HsProtobuf.Message OneInt where
        encodeMessage _ OneInt{oneIntResult = oneIntResult}
          = (Hs.mconcat
               [(HsProtobuf.encodeMessageField (HsProtobuf.FieldNumber 1)
                   oneIntResult)])
        decodeMessage _
          = (Hs.pure OneInt) <*>
              (HsProtobuf.at HsProtobuf.decodeMessageField
                 (HsProtobuf.FieldNumber 1))
        dotProto _
          = [(HsProtobuf.DotProtoField (HsProtobuf.FieldNumber 1)
                (HsProtobuf.Prim HsProtobuf.Int32)
                (HsProtobuf.Single "result")
                []
                Hs.Nothing)]
