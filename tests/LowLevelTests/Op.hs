{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module LowLevelTests.Op where

import           Control.Concurrent             (threadDelay)
import           Control.Concurrent.Async
import           Control.Monad
import           Data.ByteString                (ByteString, isPrefixOf)
import qualified Data.Map                       as M
import           Foreign.Storable               (peek)
import           Test.Tasty
import           Test.Tasty.HUnit               as HU (testCase, (@?=),
                                                       assertBool)

import           Network.GRPC.LowLevel
import           Network.GRPC.LowLevel.Call
import           Network.GRPC.LowLevel.Client
import           Network.GRPC.LowLevel.Server
import           Network.GRPC.LowLevel.Op
import           Network.GRPC.LowLevel.CompletionQueue

lowLevelOpTests :: TestTree
lowLevelOpTests = testGroup "Synchronous unit tests of low-level Op interface"
  [testCancelWhileHandling
  ,testCancelFromServer]

testCancelWhileHandling :: TestTree
testCancelWhileHandling =
  testCase "Client/Server - cancel after handler starts does nothing" $
  runSerialTest $ \grpc ->
    withClientServerUnaryCall grpc $
    \(c@Client{..}, s@Server{..}, cc@ClientCall{..}, sc@ServerCall{..}) -> do
      withOpArrayAndCtxts serverEmptyRecvOps $ \(opArray, ctxts) -> do
        tag <- newTag serverCQ
        startBatch serverCQ unServerCall opArray 3 tag
        pluck serverCQ tag (Just 1)
        let (OpRecvCloseOnServerContext pcancelled) = last ctxts
        cancelledBefore <- peek pcancelled
        cancelledBefore @?= 0
        clientCallCancel cc
        threadDelay 1000000
        cancelledAfter <- peek pcancelled
        cancelledAfter @?= 0
        return $ Right ()

testCancelFromServer :: TestTree
testCancelFromServer =
  testCase "Client/Server - client receives server cancellation" $
  runSerialTest $ \grpc ->
    withClientServerUnaryCall grpc $
    \(c@Client{..}, s@Server{..}, cc@ClientCall{..}, sc@ServerCall{..}) -> do
      serverCallCancel sc StatusPermissionDenied "TestStatus"
      clientRes <- runOps unClientCall clientCQ clientRecvOps
      case clientRes of
        Left x -> error $ "Client recv error: " ++ show x
        Right [_,_,OpRecvStatusOnClientResult _ code details] -> do
          code @?= StatusPermissionDenied
          assertBool "Received status details or RST_STREAM error" $
            details == "TestStatus"
            ||
            isPrefixOf "Received RST_STREAM" details
          return $ Right ()


runSerialTest :: (GRPC -> IO (Either GRPCIOError ())) -> IO ()
runSerialTest f =
  withGRPC f >>= \case Left x -> error $ show x
                       Right () -> return ()

withClientServerUnaryCall :: GRPC
                             -> ((Client, Server, ClientCall, ServerCall)
                                 -> IO (Either GRPCIOError a))
                             -> IO (Either GRPCIOError a)
withClientServerUnaryCall grpc f = do
  withClient grpc clientConf $ \c -> do
    crm <- clientRegisterMethod c "/foo" Normal
    withServer grpc serverConf $ \s ->
      withClientCall c crm 10 $ \cc -> do
        let srm = head (registeredMethods s)
        -- NOTE: We need to send client ops here or else `withServerCall` hangs,
        -- because registered methods try to do recv ops immediately when
        -- created. If later we want to send payloads or metadata, we'll need
        -- to tweak this.
        clientRes <- runOps (unClientCall cc) (clientCQ c) clientEmptySendOps
        withServerCall s srm 10 $ \sc ->
          f (c, s, cc, sc)

serverConf = (ServerConfig "localhost" 50051 [("/foo", Normal)])

clientConf = (ClientConfig "localhost" 50051)

clientEmptySendOps = [OpSendInitialMetadata mempty,
                      OpSendMessage "",
                      OpSendCloseFromClient]

clientRecvOps = [OpRecvInitialMetadata,
                 OpRecvMessage,
                 OpRecvStatusOnClient]

serverEmptyRecvOps = [OpSendInitialMetadata mempty,
                      OpRecvMessage,
                      OpRecvCloseOnServer]
