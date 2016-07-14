#include <string>
#include <iostream>
#include <atomic>

#include <grpc++/grpc++.h>
#include "echo.grpc.pb.h"

using namespace std;

using grpc::Server;
using grpc::ServerBuilder;
using grpc::ServerContext;
using grpc::Status;

using echo::EchoRequest;
using echo::Echo;

atomic_int reqCount;

class EchoServiceImpl final : public Echo::Service {
    Status DoEcho(ServerContext* ctx, const EchoRequest* req,
                  EchoRequest* resp) override {
      reqCount++;
      if(reqCount % 100 == 0){
        cout<<reqCount<<endl;
      }
      resp->set_message(req->message());
      return Status::OK;
    }
};

int main(){
  string server_address("localhost:50051");
  EchoServiceImpl service;

  ServerBuilder builder;
  builder.AddListeningPort(server_address, grpc::InsecureServerCredentials());
  builder.RegisterService(&service);
  unique_ptr<Server> server(builder.BuildAndStart());
  server->Wait();
  return 0;
}
