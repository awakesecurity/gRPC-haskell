-- | Low-level safe interface to gRPC. By "safe", we mean:
-- 1. all gRPC objects are guaranteed to be cleaned up correctly.
-- 2. all functions are thread-safe.
-- 3. all functions leave gRPC in a consistent, safe state.
-- These guarantees only apply to the functions exported by this module,
-- and not to helper functions in submodules that aren't exported here.

{-# LANGUAGE RecordWildCards #-}

module Network.GRPC.LowLevel (
-- * Important types
GRPC
, withGRPC
, GRPCIOError(..)
, StatusCode(..)

-- * Completion queue utilities
, CompletionQueue
, withCompletionQueue

-- * Calls
, GRPCMethodType(..)
, RegisteredMethod
, Call
, NormalRequestResult(..)

-- * Server
, ServerConfig(..)
, Server
, registeredMethods
, withServer
, serverHandleNormalRegisteredCall
, serverHandleNormalCall
, withServerCall
, withServerRegisteredCall

-- * Client
, ClientConfig(..)
, Client
, withClient
, clientRegisterMethod
, clientRegisteredRequest
, clientRequest
, withClientCall

-- * Ops
, runOps
, Op(..)
, OpRecvResult(..)

) where

import           Network.GRPC.LowLevel.GRPC
import           Network.GRPC.LowLevel.Server
import           Network.GRPC.LowLevel.CompletionQueue
import           Network.GRPC.LowLevel.Op
import           Network.GRPC.LowLevel.Client
import           Network.GRPC.LowLevel.Call

import Network.GRPC.Unsafe.Op (StatusCode(..))
