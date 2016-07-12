#include <string>
#include <iostream>

#include <grpc++/grpc++.h>

#include "echo.grpc.pb.h"

using namespace std;
using namespace echo;
using grpc::Channel;
using grpc::ClientContext;
using grpc::Status;

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

class AddClient {
public:
  AddClient(shared_ptr<Channel> chan) : stub_(Add::NewStub(chan)) {}

  AddResponse DoAdd(const uint32_t x, const uint32_t y){
    AddRequest msg;
    msg.set_addx(x);
    msg.set_addy(y);

    AddResponse resp;

    ClientContext ctx;

    stub_->DoAdd(&ctx, msg, &resp);

    return resp;
  }
private:
  unique_ptr<Add::Stub> stub_;
};

int main(){
  /*
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
*/

  AddClient client (grpc::CreateChannel("localhost:50051",
                                        grpc::InsecureChannelCredentials()));
  AddResponse answer = client.DoAdd(1,2);
  cout<<"Got answer: "<<answer.answer()<<endl;
  return 0;
}
