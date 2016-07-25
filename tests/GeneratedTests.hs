{-# LANGUAGE OverloadedStrings #-}

module GeneratedTests where

import Test.Tasty
import Test.Tasty.HUnit (testCase, (@?=))

import Data.String
import Data.Protobuf.Wire.DotProto.Generate
import qualified Data.Text as T

import Turtle

generatedTests :: TestTree
generatedTests = testGroup "Code generator tests"
  [ testServerGeneration ]

testServerGeneration :: TestTree
testServerGeneration = testCase "server generation" $ do
  mktree hsTmpDir
  mktree pyTmpDir

  compileSimpleDotProto

  exitCode <- shell (T.concat ["stack ghc -- --make -threaded -odir ", hsTmpDir, " -hidir ", hsTmpDir, " -o ", hsTmpDir, "/simple-server ", hsTmpDir, "/Simple.hs tests/TestServer.hs > /dev/null"]) empty
  exitCode @?= ExitSuccess

  exitCode <- shell (T.concat ["python -m grpc.tools.protoc -I tests --python_out=", pyTmpDir, " --grpc_python_out=", pyTmpDir, " tests/simple.proto"]) empty
  exitCode @?= ExitSuccess

  runManaged $ do
    serverExitCodeA <- fork (shell (hsTmpDir <> "/simple-server") empty)
    clientExitCodeA <- fork
      (export "PYTHONPATH" pyTmpDir >> shell "python tests/test-client.py" empty)

    liftIO $ do
      serverExitCode <- liftIO (wait serverExitCodeA)
      clientExitCode <- liftIO (wait clientExitCodeA)

      serverExitCode @?= ExitSuccess
      clientExitCode @?= ExitSuccess

  rmtree hsTmpDir
  rmtree pyTmpDir

hsTmpDir, pyTmpDir :: IsString a => a
hsTmpDir = "tests/tmp"
pyTmpDir = "tests/py-tmp"

compileSimpleDotProto :: IO ()
compileSimpleDotProto =
  do dpRes <- readDotProtoWithContext "tests/simple.proto"
     case dpRes of
       Left err -> fail (show err)
       Right (dp, ctxt) ->
         case renderHsModuleForDotProto dp ctxt of
           Left err -> fail ("compileSimpleDotProto: Error compiling test.proto: " <> show err)
           Right hsSrc -> writeFile (hsTmpDir ++ "/Simple.hs") hsSrc
