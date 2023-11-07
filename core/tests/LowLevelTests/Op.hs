{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module LowLevelTests.Op where

import Control.Concurrent
import Control.Exception
import Control.Monad
import Data.ByteString (ByteString)
import Network.GRPC.LowLevel
import Network.GRPC.LowLevel.Call
import Network.GRPC.LowLevel.Client
import Network.GRPC.LowLevel.Op
import Network.GRPC.LowLevel.Server
import Test.Tasty
import Test.Tasty.HUnit as HU (testCase, (@?=))

lowLevelOpTests :: TestTree
lowLevelOpTests =
  testGroup
    "Synchronous unit tests of low-level Op interface"
    [testCancelFromServer]

testCancelFromServer :: TestTree
testCancelFromServer =
  testCase "Client/Server - client receives server cancellation" $
    runSerialTest $ \grpc ->
      withClientServerUnaryCall grpc $
        \(Client {..}, Server {}, ClientCall {..}, sc@ServerCall {}) -> do
          serverCallCancel sc StatusPermissionDenied "TestStatus"
          clientRes <- runOps unsafeCC clientCQ clientRecvOps
          case clientRes of
            Left x -> error $ "Client recv error: " ++ show x
            Right [_, _, OpRecvStatusOnClientResult _ code _details] -> do
              code @?= StatusPermissionDenied
              return $ Right ()
            wrong -> error $ "Unexpected op results: " ++ show wrong

runSerialTest :: (GRPC -> IO (Either GRPCIOError ())) -> IO ()
runSerialTest f =
  withGRPC f >>= \case
    Left x -> error $ show x
    Right () -> return ()

withClientServerUnaryCall ::
  GRPC ->
  ( ( Client,
      Server,
      ClientCall,
      ServerCall ByteString
    ) ->
    IO (Either GRPCIOError a)
  ) ->
  IO (Either GRPCIOError a)
withClientServerUnaryCall grpc f = do
  withClient grpc clientConf $ \c -> do
    crm <- clientRegisterMethodNormal c "/foo"
    withServer grpc serverConf $ \s -> do
      ccVar <- newEmptyMVar
      bracket newEmptyMVar (\v -> putMVar v ()) $ \finished -> do
        _ <- forkIO $
          void $
            withClientCall c crm 10 $ \cc -> do
              putMVar ccVar cc
              -- NOTE: We need to send client ops here or else `withServerCall` hangs,
              -- because registered methods try to do recv ops immediately when
              -- created. If later we want to send payloads or metadata, we'll need
              -- to tweak this.
              _clientRes <- runOps (unsafeCC cc) (clientCQ c) clientEmptySendOps
              takeMVar finished
              pure (Right ())
        let srm = head (normalMethods s)
        cc <- takeMVar ccVar
        withServerCall s srm $ \sc ->
          f (c, s, cc, sc)

serverConf :: ServerConfig
serverConf = ServerConfig "localhost" 50051 [("/foo")] [] [] [] [] serverSSLConf

serverSSLConf :: ServerSSLConfig
serverSSLConf = ServerSSLConfig Nothing "tests/ssl/localhost.key" "tests/ssl/localhost.crt" SslDontRequestClientCertificate Nothing


clientConf :: ClientConfig
clientConf = ClientConfig "localhost:50051" [] clientSSLConf Nothing

clientSSLConf :: ClientSSLConfig
clientSSLConf = ClientSSLConfig (Just "tests/ssl/localhost.crt") Nothing Nothing

clientEmptySendOps :: [Op]
clientEmptySendOps =
  [ OpSendInitialMetadata mempty,
    OpSendMessage "",
    OpSendCloseFromClient
  ]

clientRecvOps :: [Op]
clientRecvOps =
  [ OpRecvInitialMetadata,
    OpRecvMessage,
    OpRecvStatusOnClient
  ]

serverEmptyRecvOps :: [Op]
serverEmptyRecvOps =
  [ OpSendInitialMetadata mempty,
    OpRecvMessage,
    OpRecvCloseOnServer
  ]
