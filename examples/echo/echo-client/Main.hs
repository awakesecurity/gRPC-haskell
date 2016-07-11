{-# LANGUAGE LambdaCase                      #-}
{-# LANGUAGE OverloadedStrings               #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# OPTIONS_GHC -fno-warn-unused-binds       #-}

import           Control.Monad
import           Network.GRPC.LowLevel
import           Network.GRPC.LowLevel.Call
import qualified Network.GRPC.LowLevel.Client.Unregistered as U
import           System.Environment

echoMethod = MethodName "/echo.Echo/DoEcho"

_unregistered c = U.clientRequest c echoMethod 1 "hi" mempty

main = withGRPC $ \g ->
  withClient g (ClientConfig "localhost" 50051 []) $ \c -> do
  rm <- clientRegisterMethod c echoMethod Normal
  replicateM_ 100000 $ clientRequest c rm 5 "hi" mempty >>= \case
    Left e -> error $ "Got client error: " ++ show e
    _      -> return ()
