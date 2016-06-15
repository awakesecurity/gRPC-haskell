#ifndef GRPC_HASKELL
#define GRPC_HASKELL

#include <grpc/grpc.h>
#include <grpc/impl/codegen/slice.h>
#include <grpc/impl/codegen/time.h>
#include <grpc/byte_buffer.h>
#include <grpc/byte_buffer_reader.h>

grpc_event *grpc_completion_queue_next_(grpc_completion_queue *cq,
                                       gpr_timespec *deadline,
                                       void *reserved);

grpc_event *grpc_completion_queue_pluck_(grpc_completion_queue *cq, void *tag,
                                        gpr_timespec *deadline,
                                        void *reserved);

grpc_call *grpc_channel_create_call_(grpc_channel *channel,
                                     grpc_call *parent_call,
                                     uint32_t propagation_mask,
                                     grpc_completion_queue *completion_queue,
                                     const char *method, const char *host,
                                     gpr_timespec *deadline, void *reserved);

size_t gpr_slice_length_(gpr_slice *slice);

uint8_t *gpr_slice_start_(gpr_slice *slice);

gpr_slice* gpr_slice_from_copied_string_(const char *source);

void free_slice(gpr_slice *slice);

grpc_byte_buffer **create_receiving_byte_buffer();

void destroy_receiving_byte_buffer(grpc_byte_buffer **bb);

grpc_byte_buffer_reader *byte_buffer_reader_create(grpc_byte_buffer *buffer);

void byte_buffer_reader_destroy(grpc_byte_buffer_reader *reader);

gpr_slice* grpc_byte_buffer_reader_readall_(grpc_byte_buffer_reader *reader);

void timespec_destroy(gpr_timespec* t);

gpr_timespec* gpr_inf_future_(gpr_clock_type t);

gpr_timespec* gpr_now_(gpr_clock_type t);

int32_t gpr_time_to_millis_(gpr_timespec* t);

gpr_timespec* seconds_to_deadline(int64_t seconds);

gpr_timespec* millis_to_deadline(int64_t millis);

gpr_timespec* infinite_deadline();

grpc_metadata_array** metadata_array_create();

void metadata_array_destroy(grpc_metadata_array **arr);

grpc_metadata* metadata_alloc(size_t n);

void metadata_free(grpc_metadata* m);

void set_metadata_key_val(char *key, char *val, grpc_metadata *arr, size_t i);

const char* get_metadata_key(grpc_metadata *arr, size_t i);

const char* get_metadata_val(grpc_metadata *arr, size_t i);

grpc_op* op_array_create(size_t n);

void op_array_destroy(grpc_op* op_array, size_t n);

void op_send_initial_metadata(grpc_op *op_array, size_t i,
                              grpc_metadata *arr, size_t n_metadata);

void op_send_initial_metadata_empty(grpc_op *op_array, size_t i);

void op_send_message(grpc_op *op_array, size_t i,
                     grpc_byte_buffer *payload);

void op_send_close_client(grpc_op *op_array, size_t i);

void op_recv_initial_metadata(grpc_op *op_array, size_t i,
                              grpc_metadata_array** arr);

void op_recv_message(grpc_op *op_array, size_t i,
                     grpc_byte_buffer **payload_recv);

void op_recv_status_client(grpc_op *op_array, size_t i,
                           grpc_metadata_array** arr,
                           grpc_status_code* status,
                           char **details, size_t* details_capacity);

void op_recv_close_server(grpc_op *op_array, size_t i, int *was_cancelled);

void op_send_status_server(grpc_op *op_array, size_t i,
                           size_t metadata_count, grpc_metadata* m,
                           grpc_status_code status, char *details);

grpc_status_code* create_status_code_ptr();

grpc_status_code deref_status_code_ptr(grpc_status_code* p);

void destroy_status_code_ptr(grpc_status_code* p);

grpc_call_details* create_call_details();

void destroy_call_details(grpc_call_details* cd);

void grpc_channel_watch_connectivity_state_(grpc_channel *channel,
                                            grpc_connectivity_state
                                            last_observed_state,
                                            gpr_timespec* deadline,
                                            grpc_completion_queue *cq,
                                            void *tag);

grpc_metadata* metadata_array_get_metadata(grpc_metadata_array* arr);

void metadata_array_set_metadata(grpc_metadata_array* arr, grpc_metadata* meta);

size_t metadata_array_get_count(grpc_metadata_array* arr);

size_t metadata_array_get_capacity(grpc_metadata_array* arr);

grpc_call* grpc_channel_create_registered_call_(
  grpc_channel *channel, grpc_call *parent_call, uint32_t propagation_mask,
  grpc_completion_queue *completion_queue, void *registered_call_handle,
  gpr_timespec *deadline, void *reserved);

char* call_details_get_method(grpc_call_details* details);

char* call_details_get_host(grpc_call_details* details);

gpr_timespec* call_details_get_deadline(grpc_call_details* details);

void* grpc_server_register_method_(grpc_server* server, const char* method,
                                   const char* host);

#endif //GRPC_HASKELL
