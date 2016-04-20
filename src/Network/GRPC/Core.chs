module Network.GRPC.Core where

-- TODO Remove wrapped function once  once https://github.com/haskell/c2hs/issues/117 gets in

import Foreign.C.String
import Foreign.C.Types
import Foreign.Ptr

import Network.GRPC.Core.Time

#include <grpc/grpc.h>
#include <grpc/status.h>
#include <grpc_haskell.h>

{#context prefix = "grpc" #}

{#pointer *gpr_timespec as CTimeSpecPtr -> CTimeSpec #}
{#enum grpc_status_code as StatusCode {underscoreToCase} deriving (Eq)#}

{#pointer *grpc_completion_queue as CompletionQueue newtype #}
{#pointer *grpc_channel as Channel newtype #}
{#pointer *grpc_server as Server newtype #}
{#pointer *grpc_call as Call newtype #}

-- {#enum grpc_arg_type as ArgType {underscoreToCase} deriving (Eq)#}

newtype ChannelArgs = ChannelArgs [Arg]

-- TODO Storable ChannelArgs

{#pointer *grpc_channel_args as ChannelArgsPtr -> ChannelArgs #}

data Arg = Arg { argKey :: String, argValue :: ArgValue }
data ArgValue = ArgString String | ArgInt Int

{#enum grpc_call_error as CallError {underscoreToCase} deriving (Eq)#}

{#pointer *grpc_byte_buffer as ByteBuffer newtype #}
{#pointer *grpc_byte_buffer_reader as ByteBufferReader newtype #}

{#enum grpc_completion_type as CompletionType {underscoreToCase} deriving (Eq)#}
{#pointer *grpc_event as Event newtype #}
{#enum grpc_op_type as OpType {underscoreToCase} deriving (Eq)#}
{#pointer *grpc_op as Op newtype #}

{#fun grpc_init as ^ {} -> `()'#}
{#fun grpc_shutdown as ^ {} -> `()'#}

{#fun grpc_completion_queue_create as ^ {`Ptr ()'} -> `CompletionQueue'#}

{#fun grpc_completion_queue_next_ as ^ {`CompletionQueue', `CTimeSpecPtr'} -> `Event'#}
{#fun grpc_completion_queue_pluck_ as ^ {`CompletionQueue', `Ptr ()', `CTimeSpecPtr'} -> `Event'#}

{#fun grpc_completion_queue_shutdown as ^ {`CompletionQueue'} -> `()'#}
{#fun grpc_completion_queue_destroy as ^ {`CompletionQueue'} -> `()'#}

{#fun grpc_channel_create_call_ as ^ {`Channel', `Call', `Int', `CompletionQueue', `String', `String', `CTimeSpecPtr', `Ptr ()'} -> `Call'#}
{#fun grpc_insecure_channel_create as ^ {`String', `ChannelArgsPtr', `Ptr ()'} -> `Channel'#}
{#fun grpc_channel_destroy as ^ {`Channel'} -> `()'#}

{#fun grpc_call_start_batch as ^ {`Call', `Op', `Int', `Ptr ()', `Ptr ()'} -> `CallError'#}
{#fun grpc_call_cancel as ^ {`Call', `Ptr ()'} -> `()'#}
{#fun grpc_call_cancel_with_status as ^ {`Call', `StatusCode', `String', `Ptr ()'} -> `()'#}
{#fun grpc_call_destroy as ^ {`Call'} -> `()'#}
