{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Prelude hiding (sum)

import Simple

import Control.Concurrent
import Control.Concurrent.MVar
import Control.Monad
import Control.Monad.IO.Class
import Control.Exception
import Control.Arrow

import Data.Monoid
import Data.Foldable (sum)
import Data.String
import Data.Word
import Data.Vector (fromList)

import Network.GRPC.LowLevel
import Network.GRPC.HighLevel.Client

import Proto3.Suite

import System.Random

import Test.Tasty
import Test.Tasty.HUnit ((@?=), assertBool, testCase)

testNormalCall client = testCase "Normal call" $
  do randoms <- fromList <$> replicateM 1000 (Fixed <$> randomRIO (1, 1000))
     let req = SimpleServiceRequest "NormalRequest" randoms
     res <- simpleServiceNormalCall client
              (ClientNormalRequest req 10 mempty)
     case res of
       ClientErrorResponse err -> assertBool ("ClientErrorResponse: " <> show err) False
       ClientNormalResponse res _ _ stsCode _ ->
         do stsCode @?= StatusOk
            simpleServiceResponseResponse res @?= "NormalRequest"
            simpleServiceResponseNum res @?= sum randoms

testClientStreamingCall client = testCase "Client-streaming call" $
  do iterationCount <- randomRIO (5, 50)
     v <- newEmptyMVar
     res <- simpleServiceClientStreamingCall client . ClientWriterRequest 10 mempty $ \send ->
       do (finalName, totalSum) <-
             fmap ((mconcat *** (sum . mconcat)) . unzip) .
             replicateM iterationCount $
             do randoms <- fromList <$> replicateM 1000 (Fixed <$> randomRIO (1, 1000))
                name <- fromString <$> replicateM 10 (randomRIO ('a', 'z'))
                send (SimpleServiceRequest name randoms)
                pure (name, randoms)
          putMVar v (finalName, totalSum)

     (finalName, totalSum) <- readMVar v
     case res of
       ClientErrorResponse err -> assertBool ("ClientErrorResponse: " <> show err) False
       ClientWriterResponse Nothing _ _ _ _ -> assertBool "No response received" False
       ClientWriterResponse (Just res) _ _ stsCode _ ->
         do stsCode @?= StatusOk
            simpleServiceResponseResponse res @?= finalName
            simpleServiceResponseNum res @?= totalSum

testServerStreamingCall client = testCase "Server-streaming call" $
  do numCount <- randomRIO (50, 500)
     nums <- replicateM numCount (Fixed <$> randomIO)

     let checkResults [] recv =
           do res <- recv
              case res of
                Left err -> assertBool ("recv error: " <> show err) False
                Right Nothing -> pure ()
                Right (Just _) -> assertBool "recv: elements past end of stream" False
         checkResults (expNum:nums) recv =
           do res <- recv
              case res of
                Left err -> assertBool ("recv error: " <> show err) False
                Right Nothing -> assertBool ("recv: stream ended earlier than expected") False
                Right (Just (SimpleServiceResponse response num)) ->
                  do response @?= "Test"
                     num @?= expNum
                     checkResults nums recv
     res <- simpleServiceServerStreamingCall client $
            ClientReaderRequest (SimpleServiceRequest "Test" (fromList nums)) 10 mempty
              (\_ -> checkResults nums)
     case res of
       ClientErrorResponse err -> assertBool ("ClientErrorResponse: " <> show err) False
       ClientReaderResponse _ sts _ ->
         sts @?= StatusOk

testBiDiStreamingCall client = testCase "Bidi-streaming call" $
  do let handleRequests (0 :: Int) _ _ done = done >> pure ()
         handleRequests n recv send done =
           do numCount <- randomRIO (10, 1000)
              nums <- fromList <$> replicateM numCount (Fixed <$> randomRIO (1, 1000))
              testName <- fromString <$> replicateM 10 (randomRIO ('a', 'z'))
              send (SimpleServiceRequest testName nums)

              res <- recv
              case res of
                Left err -> assertBool ("recv error: " <> show err) False
                Right Nothing -> pure ()
                Right (Just (SimpleServiceResponse name total)) ->
                  do name @?= testName
                     total @?= sum nums
                     handleRequests (n - 1) recv send done

     iterations <- randomRIO (50, 500)

     res <- simpleServiceBiDiStreamingCall client $
            ClientBiDiRequest 10 mempty (\_ -> handleRequests iterations)
     case res of
       ClientErrorResponse err -> assertBool ("ClientErrorResponse: " <> show err) False
       ClientBiDiResponse _ sts _ ->
         sts @?= StatusOk

main :: IO ()
main = do
  threadDelay 10000000
  withGRPC $ \grpc ->
   withClient grpc (ClientConfig "localhost" 50051 [] Nothing) $ \client ->
    do service <- simpleServiceClient client

       (defaultMain $ testGroup "Send gRPC requests"
          [ testNormalCall service
          , testClientStreamingCall service
          , testServerStreamingCall service
          , testBiDiStreamingCall service ]) `finally`
         (simpleServiceDone service (ClientNormalRequest SimpleServiceDone 10 mempty))
