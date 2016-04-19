#include <grpc/grpc.h>

grpc_event grpc_completion_queue_next_(grpc_completion_queue *cq,
                                        gpr_timespec *deadline,
                                        void *reserved) {
  return grpc_completion_queue_next(cq, *deadline, reserved);
}

grpc_event grpc_completion_queue_pluck_(grpc_completion_queue *cq, void *tag,
                                         gpr_timespec *deadline,
                                         void *reserved) {
  return grpc_completion_queue_pluck(cq, tag, *deadline, reserved);
}
