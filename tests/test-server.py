from simple_pb2 import *
import simple_pb2_grpc
from uuid import uuid4
from concurrent import futures
import random
import Queue
import grpc

print "Starting python server"

done_queue = Queue.Queue()

class SimpleServiceServer(simple_pb2_grpc.SimpleServiceServicer):
    def done(self, request, context):
        global server
        done_queue.put_nowait(())

        return SimpleServiceDone()

    def normalCall(self, request, context):
        return SimpleServiceResponse(response = "NormalRequest", num = sum(request.num))

    def clientStreamingCall(self, requests, context):
        cur_name = ""
        cur_sum = 0
        for request in requests:
            cur_name += request.request
            cur_sum += sum(request.num)
        return SimpleServiceResponse(response = cur_name, num = cur_sum)

    def serverStreamingCall(self, request, context):
        for num in request.num:
            yield SimpleServiceResponse(response = request.request, num = num)

    def biDiStreamingCall(self, requests, context):
        for request in requests:
            yield SimpleServiceResponse(response = request.request, num = sum(request.num))

server = grpc.server(futures.ThreadPoolExecutor(max_workers=1))
simple_pb2_grpc.add_SimpleServiceServicer_to_server(SimpleServiceServer(), server)
server.add_insecure_port('[::]:50051')
server.start()

done_queue.get()
