{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Network.GRPC.HighLevel.Client
  ( ClientError(..)
  , ClientRegisterable(..)
  , ClientRequest(..)
  , ClientResult(..)
  , GRPCMethodType(..)
  , MetadataMap(..)
  , RegisteredMethod
  , ServiceClient
  , StatusCode(..)
  , StatusDetails(..)
  , StreamRecv
  , StreamSend
  , TimeoutSeconds
  , WritesDone

  , LL.Client
  , LL.ClientConfig(..)
  , LL.ClientSSLConfig(..)
  , LL.ClientSSLKeyCertPair(..)
  , LL.Host(..)
  , LL.Port(..)

  , clientRequest

  -- * Client utility functions
  , acquireClient
  , simplifyUnary
  )

where

import           Control.Monad.Managed                 (Managed, liftIO,
                                                        managed)
import qualified Data.ByteString.Lazy                  as BL
import           Network.GRPC.HighLevel.Server         (convertRecv,
                                                        convertSend)
import           Network.GRPC.LowLevel                 (GRPCIOError (..),
                                                        GRPCMethodType (..),
                                                        MetadataMap (..),
                                                        StatusCode (..),
                                                        StatusDetails (..),
                                                        StreamRecv, StreamSend)
import qualified Network.GRPC.LowLevel                 as LL
import           Network.GRPC.LowLevel.CompletionQueue (TimeoutSeconds)
import           Network.GRPC.LowLevel.Op              (WritesDone)
import           Proto3.Suite                          (Message, fromByteString,
                                                        toLazyByteString)
import           Proto3.Wire.Decode                    (ParseError)

newtype RegisteredMethod (mt :: GRPCMethodType) request response
  = RegisteredMethod (LL.RegisteredMethod mt)
  deriving Show

type ServiceClient service = service ClientRequest ClientResult

data ClientError
  = ClientErrorNoParse ParseError
  | ClientIOError GRPCIOError
  deriving (Show, Eq)

data ClientRequest (streamType :: GRPCMethodType) request response where
  ClientNormalRequest :: request -> TimeoutSeconds -> MetadataMap -> ClientRequest 'Normal request response
  ClientWriterRequest :: TimeoutSeconds -> MetadataMap -> (StreamSend request -> IO ()) -> ClientRequest 'ClientStreaming request response
  -- | The final field will be invoked once, and it should repeatedly
  -- invoke its final argument (of type @(StreamRecv response)@)
  -- in order to obtain the streaming response incrementally.
  ClientReaderRequest :: request -> TimeoutSeconds -> MetadataMap -> (MetadataMap -> StreamRecv response -> IO ()) -> ClientRequest 'ServerStreaming request response
  ClientBiDiRequest :: TimeoutSeconds -> MetadataMap -> (MetadataMap -> StreamRecv response -> StreamSend request -> WritesDone -> IO ()) -> ClientRequest 'BiDiStreaming request response

data ClientResult (streamType :: GRPCMethodType) response where
  ClientNormalResponse :: response -> MetadataMap -> MetadataMap -> StatusCode -> StatusDetails -> ClientResult 'Normal response
  ClientWriterResponse :: Maybe response -> MetadataMap -> MetadataMap -> StatusCode -> StatusDetails -> ClientResult 'ClientStreaming response
  ClientReaderResponse :: MetadataMap -> StatusCode -> StatusDetails -> ClientResult 'ServerStreaming response
  ClientBiDiResponse   :: MetadataMap -> StatusCode -> StatusDetails -> ClientResult 'BiDiStreaming response
  ClientErrorResponse  :: ClientError -> ClientResult streamType response

class ClientRegisterable (methodType :: GRPCMethodType) where
  clientRegisterMethod :: LL.Client
                       -> LL.MethodName
                       -> IO (RegisteredMethod methodType request response)

instance ClientRegisterable 'Normal where
  clientRegisterMethod client methodName =
    RegisteredMethod <$> LL.clientRegisterMethodNormal client methodName

instance ClientRegisterable 'ClientStreaming where
  clientRegisterMethod client methodName =
    RegisteredMethod <$> LL.clientRegisterMethodClientStreaming client methodName

instance ClientRegisterable 'ServerStreaming where
  clientRegisterMethod client methodName =
    RegisteredMethod <$> LL.clientRegisterMethodServerStreaming client methodName

instance ClientRegisterable 'BiDiStreaming where
  clientRegisterMethod client methodName =
    RegisteredMethod <$> LL.clientRegisterMethodBiDiStreaming client methodName

clientRequest :: (Message request, Message response) =>
                 LL.Client -> RegisteredMethod streamType request response
              -> ClientRequest streamType request response -> IO (ClientResult streamType response)
clientRequest client (RegisteredMethod method) (ClientNormalRequest req timeout meta) =
    mkResponse <$> LL.clientRequest client method timeout (BL.toStrict (toLazyByteString req)) meta
  where
    mkResponse (Left ioError_) = ClientErrorResponse (ClientIOError ioError_)
    mkResponse (Right rsp) =
      case fromByteString (LL.rspBody rsp) of
        Left err -> ClientErrorResponse (ClientErrorNoParse err)
        Right parsedRsp ->
          ClientNormalResponse parsedRsp (LL.initMD rsp) (LL.trailMD rsp) (LL.rspCode rsp) (LL.details rsp)
clientRequest client (RegisteredMethod method) (ClientWriterRequest timeout meta handler) =
    mkResponse <$> LL.clientWriter client method timeout meta (handler . convertSend)
  where
    mkResponse (Left ioError_) = ClientErrorResponse (ClientIOError ioError_)
    mkResponse (Right (rsp_, initMD_, trailMD_, rspCode_, details_)) =
      case maybe (Right Nothing) (fmap Just . fromByteString) rsp_ of
        Left err -> ClientErrorResponse (ClientErrorNoParse err)
        Right parsedRsp ->
          ClientWriterResponse parsedRsp initMD_ trailMD_ rspCode_ details_
clientRequest client (RegisteredMethod method) (ClientReaderRequest req timeout meta handler) =
    mkResponse <$> LL.clientReader client method timeout (BL.toStrict (toLazyByteString req)) meta (\m recv -> handler m (convertRecv recv))
  where
    mkResponse (Left ioError_) = ClientErrorResponse (ClientIOError ioError_)
    mkResponse (Right (meta_, rspCode_, details_)) =
      ClientReaderResponse meta_ rspCode_ details_
clientRequest client (RegisteredMethod method) (ClientBiDiRequest timeout meta handler) =
    mkResponse <$> LL.clientRW client method timeout meta (\_m recv send writesDone -> handler meta (convertRecv recv) (convertSend send) writesDone)
  where
    mkResponse (Left ioError_) = ClientErrorResponse (ClientIOError ioError_)
    mkResponse (Right (meta_, rspCode_, details_)) =
      ClientBiDiResponse meta_ rspCode_ details_

acquireClient
  :: LL.ClientConfig
     -- ^ The client configuration (host, port, SSL settings, etc)
  -> (LL.Client -> IO (ServiceClient service))
     -- ^ The client implementation (typically generated)
  -> Managed (ServiceClient service)
acquireClient cfg impl = do
  g <- managed LL.withGRPC
  c <- managed (LL.withClient g cfg)
  liftIO (impl c)

-- | A utility for simplifying gRPC client requests in common cases; you can use
-- this to avoid 'ClientRequest' and 'ClientResponse' pattern-matching
-- boilerplate at call sites.
simplifyUnary :: TimeoutSeconds
                -- ^ RPC call timeout, in seconds
              -> MetadataMap
                 -- ^ RPC call metadata
              -> (ClientError -> IO (b, StatusDetails))
                 -- ^ Handler for client errors
              -> (b -> StatusCode -> StatusDetails -> IO (b, StatusDetails))
                 -- ^ Handler for non-StatusOK responses
              -> (ClientRequest 'Normal a b -> IO (ClientResult 'Normal b))
                 -- ^ gRPC function implementation (typically generated by gRPC-haskell)
              -> (a -> IO (b, StatusDetails))
                 -- ^ The simplified happy-path (StatusOk) unary call action
simplifyUnary timeout meta clientError nonStatusOkError f x = do

  let request = ClientNormalRequest x timeout meta

  response <- f request

  case response of
    ClientNormalResponse y _ _ StatusOk details
      -> pure (y, details)

    ClientNormalResponse y _ _ code details
      -> nonStatusOkError y code details

    ClientErrorResponse err
      -> clientError err
