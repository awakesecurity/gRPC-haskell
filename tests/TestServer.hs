{-# LANGUAGE OverloadedStrings #-}
module Main where

import Prelude hiding (sum)

import Simple

import Control.Concurrent
import Control.Concurrent.MVar
import Control.Monad
import Control.Monad.IO.Class

import Data.Monoid
import Data.Foldable (sum)
import Data.String

import Network.GRPC.LowLevel

handleNormalCall :: ServerCall SimpleServiceRequest -> IO (SimpleServiceResponse, MetadataMap, StatusCode, StatusDetails)
handleNormalCall call =
  pure (SimpleServiceResponse request result, mempty, StatusOk, StatusDetails "")
  where SimpleServiceRequest request nums = payload call

        result = sum nums

handleClientStreamingCall :: ServerCall () -> StreamRecv SimpleServiceRequest -> Streaming (Maybe SimpleServiceResponse, MetadataMap, StatusCode, StatusDetails)
handleClientStreamingCall call recvRequest = go 0 ""
  where go sumAccum nameAccum =
          recvRequest >>= \req ->
          case req of
            Left ioError -> pure (Nothing, mempty, StatusCancelled, StatusDetails ("handleClientStreamingCall: IO error: " <> fromString (show ioError)))
            Right Nothing ->
              pure (Just (SimpleServiceResponse nameAccum sumAccum), mempty, StatusOk, StatusDetails "")
            Right (Just (SimpleServiceRequest name nums)) ->
              go (sumAccum + sum nums) (nameAccum <> name)

handleServerStreamingCall :: ServerCall SimpleServiceRequest -> StreamSend SimpleServiceResponse -> Streaming (MetadataMap, StatusCode, StatusDetails)
handleServerStreamingCall call sendResponse = go
  where go = do forM_ nums $ \num ->
                  sendResponse (SimpleServiceResponse requestName num)
                pure (mempty, StatusOk, StatusDetails "")

        SimpleServiceRequest requestName nums = payload call

handleBiDiStreamingCall :: ServerCall () -> StreamRecv SimpleServiceRequest -> StreamSend SimpleServiceResponse -> Streaming (MetadataMap, StatusCode, StatusDetails)
handleBiDiStreamingCall call recvRequest sendResponse = go
  where go = recvRequest >>= \req ->
             case req of
               Left ioError -> pure (mempty, StatusCancelled, StatusDetails ("handleBiDiStreamingCall: IO error: " <> fromString (show ioError)))
               Right Nothing ->
                 pure (mempty, StatusOk, StatusDetails "")
               Right (Just (SimpleServiceRequest name nums)) ->
                 do sendResponse (SimpleServiceResponse name (sum nums))
                    go

handleDone :: MVar () -> ServerCall SimpleServiceDone -> IO (SimpleServiceDone, MetadataMap, StatusCode, StatusDetails)
handleDone exitVar req =
  do forkIO (threadDelay 5000 >> putMVar exitVar ())
     pure (payload req, mempty, StatusOk, StatusDetails "")

main :: IO ()
main = do exitVar <- newEmptyMVar

          forkIO $ simpleServiceServer SimpleService
            { simpleServiceDone = handleDone exitVar
            , simpleServiceNormalCall = handleNormalCall
            , simpleServiceClientStreamingCall = handleClientStreamingCall
            , simpleServiceServerStreamingCall = handleServerStreamingCall
            , simpleServiceBiDiStreamingCall = handleBiDiStreamingCall }

          takeMVar exitVar
