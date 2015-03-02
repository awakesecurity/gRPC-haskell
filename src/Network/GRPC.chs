module Network.GRPC where

import Foreign.Ptr

#include "grpc/grpc.h"

{#context prefix = "grpc" #}

{#pointer *grpc_channel as Channel newtype #}
{#pointer *grpc_server as Server newtype #}
{#pointer *grpc_call as Call newtype #}

{#enum grpc_arg_type as ArgType {underscoreToCase} deriving (Eq)#}
{#enum grpc_call_error as CallError {underscoreToCase} deriving (Eq)#}
{#enum grpc_op_error as OpError {underscoreToCase} deriving (Eq)#}

{#enum grpc_completion_type as CompletionType {underscoreToCase} deriving (Eq)#}

{#fun grpc_init as ^ {} -> `()'#}
{#fun grpc_shutdown as ^ {} -> `()'#}

