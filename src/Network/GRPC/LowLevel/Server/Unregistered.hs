{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE RecordWildCards #-}

module Network.GRPC.LowLevel.Server.Unregistered where

import           Control.Concurrent                                 (forkIO)
import           Control.Exception                                  (finally)
import           Control.Monad
import           Control.Monad.Trans.Except
import           Data.ByteString                                    (ByteString)
import           Network.GRPC.LowLevel.Call.Unregistered
import           Network.GRPC.LowLevel.CompletionQueue              (CompletionQueue
                                                                     , withCompletionQueue
                                                                     , createCompletionQueue)
import           Network.GRPC.LowLevel.CompletionQueue.Unregistered (serverRequestCall)
import           Network.GRPC.LowLevel.GRPC
import           Network.GRPC.LowLevel.Op                           (Op (..)
                                                                     , OpRecvResult (..)
                                                                     , runOps
                                                                     , runStreamingProxy
                                                                     , streamRecv
                                                                     , streamSend
                                                                     , runOps'
                                                                     , sendInitialMetadata
                                                                     , sendStatusFromServer
                                                                     , recvInitialMessage)
import           Network.GRPC.LowLevel.Server                       (Server (..)
                                                                     , ServerReaderHandler
                                                                     , ServerWriterHandler
                                                                     , ServerRWHandler)
import qualified Network.GRPC.Unsafe.Op                             as C

serverCreateCall :: Server
                 -> IO (Either GRPCIOError ServerCall)
serverCreateCall Server{..} = do
  callCQ <- createCompletionQueue serverGRPC
  serverRequestCall unsafeServer serverCQ callCQ

withServerCall :: Server
               -> (ServerCall -> IO (Either GRPCIOError a))
               -> IO (Either GRPCIOError a)
withServerCall s f =
  serverCreateCall s >>= \case
    Left e  -> return (Left e)
    Right c -> f c `finally` do
      grpcDebug "withServerCall: destroying."
      destroyServerCall c

-- | Gets a call and then forks the given function on a new thread, with the
-- new call as input. Blocks until a call is received, then returns immediately.
-- Handles cleaning up the call safely.
-- Because this function doesn't wait for the handler to return, it cannot
-- return errors.
withServerCallAsync :: Server
                       -> (ServerCall -> IO ())
                       -> IO ()
withServerCallAsync s f =
  serverCreateCall s >>= \case
    Left e -> return ()
    Right c -> void $ forkIO (f c `finally` do
      grpcDebug "withServerCallAsync: destroying."
      destroyServerCall c)

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
  =  ServerCall
  -> ByteString
  -> IO (ByteString, MetadataMap, C.StatusCode, StatusDetails)

-- | Handle one unregistered call.
serverHandleNormalCall :: Server
                       -> MetadataMap -- ^ Initial server metadata.
                       -> ServerHandler
                       -> IO (Either GRPCIOError ())
serverHandleNormalCall s initMeta f =
  withServerCall s $ \c -> serverHandleNormalCall' s c initMeta f

serverHandleNormalCall' :: Server
                        -> ServerCall
                        -> MetadataMap -- ^ Initial server metadata.
                        -> ServerHandler
                        -> IO (Either GRPCIOError ())
serverHandleNormalCall'
  s sc@ServerCall{ unsafeSC = c, callCQ = cq, .. } initMeta f = do
      grpcDebug "serverHandleNormalCall(U): starting batch."
      runOps c cq
        [ OpSendInitialMetadata initMeta
        , OpRecvMessage
        ]
        >>= \case
          Left x -> do
            grpcDebug "serverHandleNormalCall(U): ops failed; aborting"
            return $ Left x
          Right [OpRecvMessageResult (Just body)] -> do
            grpcDebug $ "got client metadata: " ++ show requestMetadataRecv
            grpcDebug $ "call_details host is: " ++ show callHost
            (rsp, trailMeta, st, ds) <- f sc body
            runOps c cq
              [ OpRecvCloseOnServer
              , OpSendMessage rsp,
                OpSendStatusFromServer trailMeta st ds
              ]
              >>= \case
                Left x -> do
                  grpcDebug "serverHandleNormalCall(U): resp failed."
                  return $ Left x
                Right _ -> do
                  grpcDebug "serverHandleNormalCall(U): ops done."
                  return $ Right ()
          x -> error $ "impossible pattern match: " ++ show x

serverReader :: Server
             -> ServerCall
             -> MetadataMap -- ^ initial server metadata
             -> ServerReaderHandler
             -> IO (Either GRPCIOError ())
serverReader s sc@ServerCall{ unsafeSC = c, callCQ = ccq } initMeta f =
  runExceptT $ do
    (mmsg, trailMeta, st, ds) <-
      runStreamingProxy "serverReader" c ccq (f (convertCall sc) streamRecv)
    runOps' c ccq ( OpSendInitialMetadata initMeta
                  : OpSendStatusFromServer trailMeta st ds
                  : maybe [] ((:[]) . OpSendMessage) mmsg
                  )
    return ()

serverWriter :: Server
             -> ServerCall
             -> MetadataMap
             -- ^ Initial server metadata
             -> ServerWriterHandler
             -> IO (Either GRPCIOError ())
serverWriter s sc@ServerCall{ unsafeSC = c, callCQ = ccq } initMeta f =
  runExceptT $ do
    bs <- recvInitialMessage c ccq
    sendInitialMetadata c ccq initMeta
    let regCall = fmap (const bs) (convertCall sc)
    st <- runStreamingProxy "serverWriter" c ccq (f regCall streamSend)
    sendStatusFromServer c ccq st

serverRW :: Server
         -> ServerCall
         -> MetadataMap
            -- ^ initial server metadata
         -> ServerRWHandler
         -> IO (Either GRPCIOError ())
serverRW s sc@ServerCall{ unsafeSC = c, callCQ = ccq } initMeta f =
  runExceptT $ do
    sendInitialMetadata c ccq initMeta
    let regCall = convertCall sc
    st <- runStreamingProxy "serverRW" c ccq (f regCall streamRecv streamSend)
    sendStatusFromServer c ccq st
