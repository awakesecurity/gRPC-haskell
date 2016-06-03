CXX = clang++
CXXFLAGS += -std=c++11
CPPFLAGS += -I/usr/local/include -pthread
LDFLAGS += -L/usr/local/lib -lgrpc++_unsecure -lgrpc -lprotobuf -lpthread -ldl

echo-server: echo.pb.o echo.grpc.pb.o echo-server.o
	clang++ $^ $(LDFLAGS) -o $@

echo-client: echo.pb.o echo.grpc.pb.o echo-client.o
	clang++ $^ $(LDFLAGS) -o $@
