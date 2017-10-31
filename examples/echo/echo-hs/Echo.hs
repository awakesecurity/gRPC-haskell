{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
-- | Generated by Haskell protocol buffer compiler. DO NOT EDIT!
module Echo where
import qualified Prelude as Hs
import qualified Proto3.Suite.DotProto as HsProtobuf
import qualified Proto3.Suite.Types as HsProtobuf
import qualified Proto3.Suite.Class as HsProtobuf
import qualified Proto3.Suite.JSONPB as HsJSONPB
import Proto3.Suite.JSONPB ((.=), (.:))
import qualified Proto3.Wire as HsProtobuf
import Control.Applicative ((<*>), (<|>), (<$>))
import qualified Control.Monad as Hs
import qualified Data.Text.Lazy as Hs (Text)
import qualified Data.ByteString as Hs
import qualified Data.String as Hs (fromString)
import qualified Data.Vector as Hs (Vector)
import qualified Data.Int as Hs (Int16, Int32, Int64)
import qualified Data.Word as Hs (Word16, Word32, Word64)
import qualified GHC.Generics as Hs
import qualified GHC.Enum as Hs
import Network.GRPC.HighLevel.Generated as HsGRPC
import Network.GRPC.HighLevel.Client as HsGRPC
import Network.GRPC.HighLevel.Server as HsGRPC hiding (serverLoop)
import Network.GRPC.HighLevel.Server.Unregistered as HsGRPC
       (serverLoop)
import Network.GRPC.LowLevel.Call as HsGRPC
 
data Echo request response = Echo{echoDoEcho ::
                                  request 'HsGRPC.Normal Echo.EchoRequest Echo.EchoResponse ->
                                    Hs.IO (response 'HsGRPC.Normal Echo.EchoResponse)}
                           deriving Hs.Generic
 
echoServer ::
             Echo HsGRPC.ServerRequest HsGRPC.ServerResponse ->
               HsGRPC.ServiceOptions -> Hs.IO ()
echoServer Echo{echoDoEcho = echoDoEcho}
  (ServiceOptions serverHost serverPort useCompression
     userAgentPrefix userAgentSuffix initialMetadata sslConfig logger)
  = (HsGRPC.serverLoop
       HsGRPC.defaultOptions{HsGRPC.optNormalHandlers =
                               [(HsGRPC.UnaryHandler (HsGRPC.MethodName "/echo.Echo/DoEcho")
                                   (HsGRPC.convertGeneratedServerHandler echoDoEcho))],
                             HsGRPC.optClientStreamHandlers = [],
                             HsGRPC.optServerStreamHandlers = [],
                             HsGRPC.optBiDiStreamHandlers = [], optServerHost = serverHost,
                             optServerPort = serverPort, optUseCompression = useCompression,
                             optUserAgentPrefix = userAgentPrefix,
                             optUserAgentSuffix = userAgentSuffix,
                             optInitialMetadata = initialMetadata, optSSLConfig = sslConfig,
                             optLogger = logger})
 
echoClient ::
             HsGRPC.Client ->
               Hs.IO (Echo HsGRPC.ClientRequest HsGRPC.ClientResult)
echoClient client
  = (Hs.pure Echo) <*>
      ((Hs.pure (HsGRPC.clientRequest client)) <*>
         (HsGRPC.clientRegisterMethod client
            (HsGRPC.MethodName "/echo.Echo/DoEcho")))
 
data EchoRequest = EchoRequest{echoRequestMessage :: Hs.Text}
                 deriving (Hs.Show, Hs.Eq, Hs.Ord, Hs.Generic)
 
instance HsProtobuf.Named EchoRequest where
        nameOf _ = (Hs.fromString "EchoRequest")
 
instance HsProtobuf.Message EchoRequest where
        encodeMessage _
          EchoRequest{echoRequestMessage = echoRequestMessage}
          = (Hs.mconcat
               [(HsProtobuf.encodeMessageField (HsProtobuf.FieldNumber 1)
                   echoRequestMessage)])
        decodeMessage _
          = (Hs.pure EchoRequest) <*>
              (HsProtobuf.at HsProtobuf.decodeMessageField
                 (HsProtobuf.FieldNumber 1))
        dotProto _
          = [(HsProtobuf.DotProtoField (HsProtobuf.FieldNumber 1)
                (HsProtobuf.Prim HsProtobuf.String)
                (HsProtobuf.Single "message")
                []
                Hs.Nothing)]
 
instance HsJSONPB.ToJSONPB EchoRequest where
        toJSONPB (EchoRequest f1) = (HsJSONPB.object ["message" .= f1])
        toEncodingPB (EchoRequest f1) = (HsJSONPB.pairs ["message" .= f1])
 
instance HsJSONPB.FromJSONPB EchoRequest where
        parseJSONPB
          = (HsJSONPB.withObject "EchoRequest"
               (\ obj -> (Hs.pure EchoRequest) <*> obj .: "message"))
 
instance HsJSONPB.ToJSON EchoRequest where
        toJSON = HsJSONPB.toAesonValue
        toEncoding = HsJSONPB.toAesonEncoding
 
instance HsJSONPB.FromJSON EchoRequest where
        parseJSON = HsJSONPB.parseJSONPB
 
instance HsJSONPB.ToSchema EchoRequest where
        declareNamedSchema = HsJSONPB.genericDeclareNamedSchemaJSONPB
 
data EchoResponse = EchoResponse{echoResponseMessage :: Hs.Text}
                  deriving (Hs.Show, Hs.Eq, Hs.Ord, Hs.Generic)
 
instance HsProtobuf.Named EchoResponse where
        nameOf _ = (Hs.fromString "EchoResponse")
 
instance HsProtobuf.Message EchoResponse where
        encodeMessage _
          EchoResponse{echoResponseMessage = echoResponseMessage}
          = (Hs.mconcat
               [(HsProtobuf.encodeMessageField (HsProtobuf.FieldNumber 1)
                   echoResponseMessage)])
        decodeMessage _
          = (Hs.pure EchoResponse) <*>
              (HsProtobuf.at HsProtobuf.decodeMessageField
                 (HsProtobuf.FieldNumber 1))
        dotProto _
          = [(HsProtobuf.DotProtoField (HsProtobuf.FieldNumber 1)
                (HsProtobuf.Prim HsProtobuf.String)
                (HsProtobuf.Single "message")
                []
                Hs.Nothing)]
 
instance HsJSONPB.ToJSONPB EchoResponse where
        toJSONPB (EchoResponse f1) = (HsJSONPB.object ["message" .= f1])
        toEncodingPB (EchoResponse f1) = (HsJSONPB.pairs ["message" .= f1])
 
instance HsJSONPB.FromJSONPB EchoResponse where
        parseJSONPB
          = (HsJSONPB.withObject "EchoResponse"
               (\ obj -> (Hs.pure EchoResponse) <*> obj .: "message"))
 
instance HsJSONPB.ToJSON EchoResponse where
        toJSON = HsJSONPB.toAesonValue
        toEncoding = HsJSONPB.toAesonEncoding
 
instance HsJSONPB.FromJSON EchoResponse where
        parseJSON = HsJSONPB.parseJSONPB
 
instance HsJSONPB.ToSchema EchoResponse where
        declareNamedSchema = HsJSONPB.genericDeclareNamedSchemaJSONPB