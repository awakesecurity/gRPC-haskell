module Network.GRPC.LowLevel.Op.Unregistered where

import           Network.GRPC.LowLevel.GRPC
import           Network.GRPC.LowLevel.Op
import           Network.GRPC.LowLevel.CompletionQueue
import           Network.GRPC.LowLevel.Call.Unregistered as U

runServerOps :: U.ServerCall
             -> CompletionQueue
             -> [Op]
             -> TimeoutSeconds
             -> IO (Either GRPCIOError [OpRecvResult])
runServerOps = runOps . U.unServerCall
