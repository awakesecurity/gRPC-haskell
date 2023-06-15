
{-# LANGUAGE OverloadedStrings #-}

module GeneratedTests where

import Test.Tasty
import Test.Tasty.HUnit (assertEqual, testCase, (@?=))

import Data.String
import Proto3.Suite.DotProto.Generate

import Turtle hiding (err)

generatedTests :: TestTree
generatedTests = testGroup "Code generator tests"
  [ testServerGeneration
  , testClientGeneration ]

testServerGeneration :: TestTree
testServerGeneration = testCase "server generation" $ do
  mktree hsTmpDir
  mktree pyTmpDir

  let args = CompileArgs
        { includeDir = ["tests"]
        , extraInstanceFiles = []
        , inputProto = "simple.proto"
        , outputDir = hsTmpDir
        , stringType = StringType "Data.Text.Lazy" "Text"
        , recordStyle = LargeRecords
        }
  compileDotProtoFileOrDie args

  do exitCode <- proc "tests/simple-server.sh" [hsTmpDir] empty
     exitCode @?= ExitSuccess

  do exitCode <- proc "tests/protoc.sh" [pyTmpDir] empty
     exitCode @?= ExitSuccess

  runManaged $ do
    serverExitCodeA <- fork (shell (hsTmpDir <> "/simple-server") empty)
    clientExitCodeA <- fork
      (export "PYTHONPATH" pyTmpDir >> shell "tests/test-client.sh" empty)

    liftIO $ do
      serverExitCode <- liftIO (wait serverExitCodeA)
      clientExitCode <- liftIO (wait clientExitCodeA)

      serverExitCode @?= ExitSuccess
      clientExitCode @?= ExitSuccess

  rmtree hsTmpDir
  rmtree pyTmpDir

testClientGeneration :: TestTree
testClientGeneration = testCase "client generation" $ do
  mktree hsTmpDir
  mktree pyTmpDir

  let args = CompileArgs
        { includeDir = ["tests"]
        , extraInstanceFiles = []
        , inputProto = "simple.proto"
        , outputDir = hsTmpDir
        , stringType = StringType "Data.Text.Lazy" "Text"
        , recordStyle = LargeRecords
        }
  compileDotProtoFileOrDie args

  do exitCode <- proc "tests/simple-client.sh" [hsTmpDir] empty
     exitCode @?= ExitSuccess

  do exitCode <- proc "tests/protoc.sh" [pyTmpDir] empty
     exitCode @?= ExitSuccess

  runManaged $ do
    serverExitCodeA <- fork
      (export "PYTHONPATH" pyTmpDir >> shell "tests/test-server.sh" empty)
    clientExitCodeA <- fork (shell (hsTmpDir <> "/simple-client") empty)

    liftIO $ do
      serverExitCode <- liftIO (wait serverExitCodeA)
      clientExitCode <- liftIO (wait clientExitCodeA)

      assertEqual "Server exit code" serverExitCode ExitSuccess
      assertEqual "Client exit code" clientExitCode ExitSuccess

  rmtree hsTmpDir
  rmtree pyTmpDir

hsTmpDir, pyTmpDir :: IsString a => a
hsTmpDir = "tests/tmp"
pyTmpDir = "tests/py-tmp"
