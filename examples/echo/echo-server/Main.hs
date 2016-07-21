{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# OPTIONS_GHC -fno-warn-unused-binds       #-}

import           Control.Concurrent
import           Control.Concurrent.Async
import           Control.Monad
import           Data.ByteString                           (ByteString)
import           Data.Protobuf.Wire.Class
import           Data.Protobuf.Wire.Types
import qualified Data.Text                                 as T
import           Data.Word
import           GHC.Generics                              (Generic)
import           Network.GRPC.HighLevel.Server
import qualified Network.GRPC.HighLevel.Server.Unregistered as U
import           Network.GRPC.LowLevel
import qualified Network.GRPC.LowLevel.Call.Unregistered   as U
import qualified Network.GRPC.LowLevel.Server.Unregistered as U

serverMeta :: MetadataMap
serverMeta = [("test_meta", "test_meta_value")]

handler :: U.ServerCall
           -> ByteString
           -> IO (ByteString, MetadataMap, StatusCode, StatusDetails)
handler U.ServerCall{..} reqBody = do
  --putStrLn $ "Got request for method: " ++ show method
  --putStrLn $ "Got metadata: " ++ show reqMeta
  return (reqBody, serverMeta, StatusOk, StatusDetails "")

unregMain :: IO ()
unregMain = withGRPC $ \grpc -> do
  withServer grpc defConfig $ \server -> forever $ do
    result <- U.serverHandleNormalCall server serverMeta handler
    case result of
      Left x -> putStrLn $ "handle call result error: " ++ show x
      Right _ -> return ()

regMain :: IO ()
regMain = withGRPC $ \grpc -> do
  let ms = [(MethodName "/echo.Echo/DoEcho")]
  withServer grpc (defConfig {methodsToRegisterNormal = ms}) $ \server ->
    forever $ do
      let method = head (normalMethods server)
      result <- serverHandleNormalCall server method serverMeta $
        \call -> return (payload call, serverMeta, StatusOk, StatusDetails "")
      case result of
        Left x -> putStrLn $ "registered call result error: " ++ show x
        Right _ -> return ()

tputStrLn x = do
  tid <- myThreadId
  putStrLn $ "[" ++ show tid ++ "]: " ++ x

regLoop :: Server -> RegisteredMethod 'Normal -> IO ()
regLoop server method = forever $ do
--  tputStrLn "about to block on call handler"
  result <- serverHandleNormalCall server method serverMeta $
    \call ->
      return (payload call, serverMeta, StatusOk, StatusDetails "")
  case result of
    Left x -> error $! "registered call result error: " ++ show x
    Right _ -> return ()

regMainThreaded :: IO ()
regMainThreaded = do
  withGRPC $ \grpc -> do
    let ms = [(MethodName "/echo.Echo/DoEcho")]
    withServer grpc (defConfig {methodsToRegisterNormal = ms}) $ \server -> do
      let method = head (normalMethods server)
      tids <- replicateM 7 $ async $ do tputStrLn "starting handler"
                                        regLoop server method
      _ <- waitAnyCancel tids
      tputStrLn "finishing"

-- NB: If you change these, make sure to change them in the client as well.
-- TODO: Put these in a common location (or just hack around it until CG is working)
data EchoRequest = EchoRequest {message :: T.Text} deriving (Show, Eq, Ord, Generic)
instance Message EchoRequest

echoHandler :: Handler 'Normal
echoHandler =
        UnaryHandler "/echo.Echo/DoEcho" $
          \call -> do
            return ( payload call :: EchoRequest
                   , metadata call
                   , StatusOk
                   , StatusDetails ""
                   )

data AddRequest = AddRequest {addX :: Fixed Word32
                              , addY :: Fixed Word32}
  deriving (Show, Eq, Ord, Generic)
instance Message AddRequest
data AddResponse = AddResponse {answer :: Fixed Word32}
  deriving (Show, Eq, Ord, Generic)
instance Message AddResponse

addHandler :: Handler 'Normal
addHandler =
  UnaryHandler "/echo.Add/DoAdd" $
    \c -> do
      let b = payload c
      return ( AddResponse $ addX b + addY b
             , metadata c
             , StatusOk
             , StatusDetails ""
             )

highlevelMain :: IO ()
highlevelMain =
  serverLoop defaultOptions{optNormalHandlers = [echoHandler, addHandler]}

highlevelMainUnregistered :: IO ()
highlevelMainUnregistered =
  U.serverLoop defaultOptions{optNormalHandlers = [echoHandler, addHandler]}

main :: IO ()
main = highlevelMainUnregistered

defConfig :: ServerConfig
defConfig = ServerConfig "localhost" 50051 [] [] [] [] []
