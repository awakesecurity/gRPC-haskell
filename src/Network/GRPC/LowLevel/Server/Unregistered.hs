{-# LANGUAGE RecordWildCards #-}

module Network.GRPC.LowLevel.Server.Unregistered where

import           Control.Exception                                  (finally)
import           Data.ByteString                                    (ByteString)
import           Network.GRPC.LowLevel.Call.Unregistered
import           Network.GRPC.LowLevel.CompletionQueue.Unregistered (serverRequestCall)
import           Network.GRPC.LowLevel.GRPC
import           Network.GRPC.LowLevel.Op                           (Op(..), OpRecvResult (..), runOps)
import           Network.GRPC.LowLevel.Server                       (Server (..))
import qualified Network.GRPC.Unsafe.Op                             as C

serverCreateCall :: Server -> IO (Either GRPCIOError ServerCall)
serverCreateCall Server{..} =
  serverRequestCall internalServer serverCQ

withServerCall :: Server
                  -> (ServerCall -> IO (Either GRPCIOError a))
                  -> IO (Either GRPCIOError a)
withServerCall server f = do
  createResult <- serverCreateCall server
  case createResult of
    Left x -> return $ Left x
    Right call -> f call `finally` logDestroy call
      where logDestroy c = grpcDebug "withServerCall: destroying."
                           >> destroyServerCall c

-- | Sequence of 'Op's needed to receive a normal (non-streaming) call.
-- TODO: We have to put 'OpRecvCloseOnServer' in the response ops, or else the
-- client times out. Given this, I have no idea how to check for cancellation on
-- the server.
serverOpsGetNormalCall :: MetadataMap -> [Op]
serverOpsGetNormalCall initMetadata =
  [OpSendInitialMetadata initMetadata,
   OpRecvMessage]

-- | Sequence of 'Op's needed to respond to a normal (non-streaming) call.
serverOpsSendNormalResponse :: ByteString
                               -> MetadataMap
                               -> C.StatusCode
                               -> StatusDetails
                               -> [Op]
serverOpsSendNormalResponse body metadata code details =
  [OpRecvCloseOnServer,
   OpSendMessage body,
   OpSendStatusFromServer metadata code details]

-- | A handler for an unregistered server call; bytestring arguments are the
-- request body and response body respectively.
type ServerHandler
  =  ServerCall -> ByteString
  -> IO (ByteString, MetadataMap, C.StatusCode, StatusDetails)

-- | Handle one unregistered call.
serverHandleNormalCall :: Server
                       -> MetadataMap -- ^ Initial server metadata.
                       -> ServerHandler
                       -> IO (Either GRPCIOError ())
serverHandleNormalCall s@Server{..} srvMetadata f = do
  withServerCall s $ \call@ServerCall{..} -> do
    grpcDebug "serverHandleNormalCall(U): starting batch."
    let recvOps = serverOpsGetNormalCall srvMetadata
    opResults <- runOps unServerCall serverCQ recvOps
    case opResults of
      Left x -> do grpcDebug "serverHandleNormalCall(U): ops failed; aborting"
                   return $ Left x
      Right [OpRecvMessageResult (Just body)] -> do
        grpcDebug $ "got client metadata: " ++ show requestMetadataRecv
        grpcDebug $ "call_details host is: " ++ show callHost
        (respBody, respMetadata, status, details) <- f call body
        let respOps = serverOpsSendNormalResponse
                        respBody respMetadata status details
        respOpsResults <- runOps unServerCall serverCQ respOps
        case respOpsResults of
          Left x -> do grpcDebug "serverHandleNormalCall(U): resp failed."
                       return $ Left x
          Right _ -> grpcDebug "serverHandleNormalCall(U): ops done."
                     >> return (Right ())
      x -> error $ "impossible pattern match: " ++ show x
