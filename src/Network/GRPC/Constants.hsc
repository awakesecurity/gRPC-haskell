module Network.GRPC.Constants where

#include "grpc/grpc.h"

argEnableCensus = #const GRPC_ARG_ENABLE_CENSUS
argMaxConcurrentStreams = #const GRPC_ARG_MAX_CONCURRENT_STREAMS
argMaxMessageLength = #const GRPC_ARG_MAX_MESSAGE_LENGTH

writeBufferHint :: Int
writeBufferHint = #const GRPC_WRITE_BUFFER_HINT

writeNoCompress :: Int
writeNoCompress = #const GRPC_WRITE_NO_COMPRESS
