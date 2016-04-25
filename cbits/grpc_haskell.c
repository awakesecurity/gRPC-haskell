#include <grpc/grpc.h>
#include <grpc/impl/codegen/grpc_types.h>
#include <stdio.h>
#include <stdlib.h>
#include <grpc_haskell.h>

grpc_event *grpc_completion_queue_next_(grpc_completion_queue *cq,
                                        gpr_timespec *deadline,
                                        void *reserved) {
  grpc_event *toReturn = malloc(sizeof(grpc_event));
  *toReturn = grpc_completion_queue_next(cq, *deadline, reserved);
  return toReturn;
}

grpc_event *grpc_completion_queue_pluck_(grpc_completion_queue *cq, void *tag,
                                         gpr_timespec *deadline,
                                         void *reserved) {
  grpc_event *toReturn = malloc(sizeof(grpc_event));
  *toReturn = grpc_completion_queue_pluck(cq, tag, *deadline, reserved);
  return toReturn;
}

grpc_call *grpc_channel_create_call_(grpc_channel *channel,
                                     grpc_call *parent_call,
                                     uint32_t propagation_mask,
                                     grpc_completion_queue *completion_queue,
                                     const char *method, const char *host,
                                     gpr_timespec *deadline, void *reserved) {
  return grpc_channel_create_call(channel, parent_call, propagation_mask,
                                       completion_queue, method, host,
                                       *deadline, reserved);
}
