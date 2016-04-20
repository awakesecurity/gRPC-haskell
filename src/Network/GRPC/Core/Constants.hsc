module Network.GRPC.Core.Constants where

#include "grpc/grpc.h"
#include "grpc/impl/codegen/propagation_bits.h"

argEnableCensus :: Int
argEnableCensus = #const GRPC_ARG_ENABLE_CENSUS

argMaxConcurrentStreams :: Int
argMaxConcurrentStreams = #const GRPC_ARG_MAX_CONCURRENT_STREAMS

argMaxMessageLength :: Int
argMaxMessageLength = #const GRPC_ARG_MAX_MESSAGE_LENGTH

writeBufferHint :: Int
writeBufferHint = #const GRPC_WRITE_BUFFER_HINT

writeNoCompress :: Int
writeNoCompress = #const GRPC_WRITE_NO_COMPRESS

newtype PropagationMask = PropagationMask {unPropagationMask :: Int}
  deriving (Show, Eq, Ord)

propagateDeadline :: Int
propagateDeadline = #const GRPC_PROPAGATE_DEADLINE

propagateCensusStatsContext :: Int
propagateCensusStatsContext = #const GRPC_PROPAGATE_CENSUS_STATS_CONTEXT

propagateCensusTracingContext :: Int
propagateCensusTracingContext = #const GRPC_PROPAGATE_CENSUS_TRACING_CONTEXT

propagateCancellation :: Int
propagateCancellation = #const GRPC_PROPAGATE_CANCELLATION
