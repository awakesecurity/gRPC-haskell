#include <grpc/grpc.h>
#include <grpc/byte_buffer.h>
#include <grpc/byte_buffer_reader.h>
#include <grpc/impl/codegen/grpc_types.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
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

size_t gpr_slice_length_(gpr_slice *slice){
  return GPR_SLICE_LENGTH(*slice);
}

uint8_t *gpr_slice_start_(gpr_slice *slice){
  return GPR_SLICE_START_PTR(*slice);
}

gpr_slice* gpr_slice_from_copied_string_(const char *source){
  gpr_slice* retval = malloc(sizeof(gpr_slice));
  //note: 'gpr_slice_from_copied_string' handles allocating space for 'source'.
  *retval = gpr_slice_from_copied_string(source);
  return retval;
}

void free_slice(gpr_slice *slice){
  gpr_slice_unref(*slice);
  free(slice);
}

grpc_byte_buffer **create_receiving_byte_buffer(){
  grpc_byte_buffer **retval = malloc(sizeof(grpc_byte_buffer*));
  *retval = NULL;
  return retval;
}

void destroy_receiving_byte_buffer(grpc_byte_buffer **bb){
  grpc_byte_buffer_destroy(*bb);
  free(bb);
}

grpc_byte_buffer_reader *byte_buffer_reader_create(grpc_byte_buffer *buffer){
  grpc_byte_buffer_reader *reader = malloc(sizeof(grpc_byte_buffer_reader));
  grpc_byte_buffer_reader_init(reader, buffer);
  return reader;
}

void byte_buffer_reader_destroy(grpc_byte_buffer_reader *reader){
  grpc_byte_buffer_reader_destroy(reader);
  free(reader);
}

gpr_slice *grpc_byte_buffer_reader_readall_(grpc_byte_buffer_reader *reader){
  gpr_slice *retval = malloc(sizeof(gpr_slice));
  *retval = grpc_byte_buffer_reader_readall(reader);
  return retval;
}

void timespec_destroy(gpr_timespec* t){
  free(t);
}

gpr_timespec* gpr_inf_future_(gpr_clock_type t){
  gpr_timespec *retval = malloc(sizeof(gpr_timespec));
  *retval = gpr_inf_future(t);
  return retval;
}

gpr_timespec* gpr_now_(gpr_clock_type t){
  gpr_timespec *retval = malloc(sizeof(gpr_timespec));
  *retval = gpr_now(t);
  return retval;
}

int32_t gpr_time_to_millis_(gpr_timespec* t){
  return gpr_time_to_millis(*t);
}

gpr_timespec* seconds_to_deadline(int64_t seconds){
  gpr_timespec *retval = malloc(sizeof(gpr_timespec));
  *retval = gpr_time_add(gpr_now(GPR_CLOCK_MONOTONIC),
                         gpr_time_from_millis(seconds * 1e3, GPR_TIMESPAN));
  return retval;
}

gpr_timespec* millis_to_deadline(int64_t millis){
  gpr_timespec *retval = malloc(sizeof(gpr_timespec));
  *retval = gpr_time_add(gpr_now(GPR_CLOCK_MONOTONIC),
                         gpr_time_from_micros(millis * 1e3, GPR_TIMESPAN));
  return retval;
}

grpc_metadata_array** metadata_array_create(){
  grpc_metadata_array **retval = malloc(sizeof(grpc_metadata_array*));
  *retval = malloc(sizeof(grpc_metadata_array));
  grpc_metadata_array_init(*retval);
  return retval;
}

void metadata_array_destroy(grpc_metadata_array **arr){
  grpc_metadata_array_destroy(*arr);
  free(*arr);
  free(arr);
}

grpc_metadata* metadata_alloc(size_t n){
  grpc_metadata *retval = malloc(sizeof(grpc_metadata)*n);
  return retval;
}

void metadata_free(grpc_metadata* m){
  free(m);
}

void set_metadata_key_val(char *key, char *val, grpc_metadata *arr, size_t i){
  grpc_metadata *p = arr + i;
  p->key = key;
  p->value = val;
  p->value_length = strlen(val);
}

const char* get_metadata_key(grpc_metadata *arr, size_t i){
  grpc_metadata *p = arr + i;
  return p->key;
}

const char* get_metadata_val(grpc_metadata *arr, size_t i){
  grpc_metadata *p = arr + i;
  return p->value;
}

grpc_op* op_array_create(size_t n){
  return malloc(n*sizeof(grpc_op));
}

void op_array_destroy(grpc_op* op_array, size_t n){
  for(int i = 0; i < n; i++){
    grpc_op* op = op_array + i;
    switch (op->op) {
      case GRPC_OP_SEND_INITIAL_METADATA:
      if(op->data.send_initial_metadata.count > 0){
        metadata_free(op->data.send_initial_metadata.metadata);
      }
      break;
      case GRPC_OP_SEND_MESSAGE:
      grpc_byte_buffer_destroy(op->data.send_message);
      break;
      case GRPC_OP_SEND_CLOSE_FROM_CLIENT:
      break;
      case GRPC_OP_SEND_STATUS_FROM_SERVER:
      free(op->data.send_status_from_server.trailing_metadata);
      free(op->data.send_status_from_server.status_details);
      break;
      case GRPC_OP_RECV_INITIAL_METADATA:
      break;
      case GRPC_OP_RECV_MESSAGE:
      break;
      case GRPC_OP_RECV_STATUS_ON_CLIENT:
      break;
      case GRPC_OP_RECV_CLOSE_ON_SERVER:
      break;
    }
  }
  free(op_array);
}

void op_send_initial_metadata(grpc_op *op_array, size_t i,
                              grpc_metadata *arr, size_t n_metadata){
  grpc_op *op = op_array + i;
  op->op = GRPC_OP_SEND_INITIAL_METADATA;
  op->data.send_initial_metadata.count = n_metadata;
  op->data.send_initial_metadata.metadata
    = malloc(n_metadata*sizeof(grpc_metadata));
  memcpy(op->data.send_initial_metadata.metadata, arr,
         n_metadata*sizeof(grpc_metadata));
  op->flags = 0;
  op->reserved = NULL;
}

void op_send_initial_metadata_empty(grpc_op *op_array, size_t i){
  grpc_op *op = op_array + i;
  op->op = GRPC_OP_SEND_INITIAL_METADATA;
  op->data.send_initial_metadata.count = 0;
  op->flags = 0;
  op->reserved = NULL;
}

void op_send_message(grpc_op *op_array, size_t i,
                         grpc_byte_buffer *payload){
  grpc_op *op = op_array + i;
  op->op = GRPC_OP_SEND_MESSAGE;
  op->data.send_message = grpc_byte_buffer_copy(payload);
  op->flags = 0;
  op->reserved = NULL;
}

void op_send_close_client(grpc_op *op_array, size_t i){
  grpc_op *op = op_array + i;
  op->op = GRPC_OP_SEND_CLOSE_FROM_CLIENT;
  op->flags = 0;
  op->reserved = NULL;
}

void op_recv_initial_metadata(grpc_op *op_array, size_t i,
                              grpc_metadata_array** arr){
  grpc_op *op = op_array + i;
  op->op = GRPC_OP_RECV_INITIAL_METADATA;
  op->data.recv_initial_metadata = *arr;
  op->flags = 0;
  op->reserved = NULL;
}

void op_recv_message(grpc_op *op_array, size_t i,
                     grpc_byte_buffer **payload_recv){
  grpc_op *op = op_array + i;
  op->op = GRPC_OP_RECV_MESSAGE;
  op->data.recv_message = payload_recv;
  op->flags = 0;
  op->reserved = NULL;
}

void op_recv_status_client(grpc_op *op_array, size_t i,
                           grpc_metadata_array** arr,
                           grpc_status_code* status,
                           char **details, size_t* details_capacity){
  grpc_op *op = op_array + i;
  op->op = GRPC_OP_RECV_STATUS_ON_CLIENT;
  op->data.recv_status_on_client.trailing_metadata = *arr;
  op->data.recv_status_on_client.status = status;
  op->data.recv_status_on_client.status_details = details;
  op->data.recv_status_on_client.status_details_capacity = details_capacity;
  op->flags = 0;
  op->reserved = NULL;
}

void op_recv_close_server(grpc_op *op_array, size_t i, int *was_cancelled){
  grpc_op *op = op_array + i;
  op->op = GRPC_OP_RECV_CLOSE_ON_SERVER;
  op->data.recv_close_on_server.cancelled = was_cancelled;
  op->flags = 0;
  op->reserved = NULL;
}

void op_send_status_server(grpc_op *op_array, size_t i,
                           size_t metadata_count, grpc_metadata* m,
                           grpc_status_code status, char *details){
  grpc_op *op = op_array + i;
  op->op = GRPC_OP_SEND_STATUS_FROM_SERVER;
  op->data.send_status_from_server.trailing_metadata_count = metadata_count;
  op->data.send_status_from_server.trailing_metadata
    = malloc(sizeof(grpc_metadata)*metadata_count);
  memcpy(op->data.send_status_from_server.trailing_metadata, m,
         metadata_count*sizeof(grpc_metadata));
  op->data.send_status_from_server.status = status;
  op->data.send_status_from_server.status_details
    = malloc(sizeof(char)*(strlen(details) + 1));
  strcpy(op->data.send_status_from_server.status_details, details);
  op->flags = 0;
  op->reserved = NULL;
}

void op_send_ok_status_server(grpc_op *op_array, size_t i){
  grpc_op *op = op_array + i;
  op->op = GRPC_OP_SEND_STATUS_FROM_SERVER;
  op->data.send_status_from_server.trailing_metadata_count = 0;
  op->data.send_status_from_server.status = GRPC_STATUS_OK;
  op->data.send_status_from_server.status_details = "OK";
  op->flags = 0;
  op->reserved = NULL;
}

grpc_status_code* create_status_code_ptr(){
  return malloc(sizeof(grpc_status_code));
}

void destroy_status_code_ptr(grpc_status_code* p){
  free(p);
}

grpc_call_details* create_call_details(){
  grpc_call_details* retval = malloc(sizeof(grpc_call_details));
  grpc_call_details_init(retval);
  return retval;
}

void destroy_call_details(grpc_call_details* cd){
  grpc_call_details_destroy(cd);
  free(cd);
}

void grpc_channel_watch_connectivity_state_(grpc_channel *channel,
                                            grpc_connectivity_state
                                            last_observed_state,
                                            gpr_timespec* deadline,
                                            grpc_completion_queue *cq,
                                            void *tag){
  grpc_channel_watch_connectivity_state(channel, last_observed_state, *deadline,
                                        cq, tag);
}
