#include <string>
#include <iostream>

#include <grpc++/grpc++.h>

#include "echo.grpc.pb.h"

using namespace std;

using grpc::Channel;
using grpc::ClientContext;
using grpc::Status;
using echo::EchoRequest;
using echo::Echo;

class EchoClient {
public:
  EchoClient(shared_ptr<Channel> chan) : stub_(Echo::NewStub(chan)) {}

  Status DoEcho(const string& msg){
    EchoRequest req;
    req.set_message(msg);

    EchoRequest resp;

    ClientContext ctx;

    return stub_->DoEcho(&ctx, req, &resp);
  }

private:
  unique_ptr<Echo::Stub> stub_;
};

int main(){
  EchoClient client(grpc::CreateChannel("localhost:50051",
                                        grpc::InsecureChannelCredentials()));
  string msg("hi");
  for(int i = 0; i < 100000; i++){
    Status status = client.DoEcho(msg);
    if(!status.ok()){
      cout<<"Error: "<<status.error_code()<<endl;
      return 1;
    }
  }

  return 0;
}
