#include <grpc/grpc.h>

grpc_event *grpc_completion_queue_next_(grpc_completion_queue *cq,
                                       gpr_timespec *deadline);

grpc_event *grpc_completion_queue_pluck_(grpc_completion_queue *cq, void *tag,
                                        gpr_timespec *deadline);

grpc_call *grpc_channel_create_call_(grpc_channel *channel,
                                     grpc_call *parent_call,
                                     uint32_t propagation_mask,
                                     grpc_completion_queue *completion_queue,
                                     const char *method, const char *host,
                                     gpr_timespec *deadline, void *reserved);
