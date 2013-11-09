#include <node.h>
#include "ipcconn.h"
#include "node_buffer.h"

using namespace v8;

Persistent<Function> IpcConn::constructor;


IpcConn::IpcConn() {

}

IpcConn::~IpcConn() {

}

void IpcConn::Init(Handle<Object> exports) {
	// Prepare constructor template
	Local<FunctionTemplate> tpl = FunctionTemplate::New(New);
	tpl->SetClassName(String::NewSymbol("IpcConn"));
	tpl->InstanceTemplate()->SetInternalFieldCount(2);

	//Prototype
	tpl->PrototypeTemplate()->Set(String::NewSymbol("resp"),
		FunctionTemplate::New(Resp)->GetFunction());
	tpl->PrototypeTemplate()->Set(String::NewSymbol("call"),
		FunctionTemplate::New(Call)->GetFunction());

	constructor = Persistent<Function>::New(tpl->GetFunction());
	exports->Set(String::NewSymbol("IpcConn"), constructor);
}

Handle<Value> IpcConn::New(const Arguments& args) {
	HandleScope scope;

	if (args.IsConstructCall()) {
		//printf("construct call\n");
		
		IpcConn *obj = new IpcConn();
		obj->Wrap(args.This());
		return args.This();
	} else {
		printf("non construct call");
		const int argc = 0;
		Local<Value> argv[argc] = { };
		return scope.Close(constructor->NewInstance());
	}
}

Handle<Object> IpcConn::NewInstance() {
	HandleScope scope;

	//const unsigned argc = 0;
	//Local<Value> argv[argc] = { };

	return scope.Close(constructor->NewInstance());
}

Handle<Value> IpcConn::Call(const v8::Arguments& args) {
	HandleScope scope;

	struct my_proto proto;
	memset(&proto, 0, sizeof(proto));
	
	proto.nodeerl_req = 1;
	int64_t pid_input = (int64_t)args[0]->NumberValue();

	size_t buffersize = node::Buffer::Length(args[1]->ToObject());
	char* bufferdata = node::Buffer::Data(args[1]->ToObject());

	proto.len = buffersize;
		
	memcpy(proto.message, bufferdata, buffersize);

	Local<Object> self = args.Holder();
	Local<External> wrap = Local<External>::Cast(self->GetInternalField(0));
	Local<External> wrap1 = Local<External>::Cast(self->GetInternalField(1));

	qb_ipcs_connection_t * c = (qb_ipcs_connection_t *)(wrap->Value());

	int64_t* pid = (int64_t*)(wrap1->Value());

	proto.pid = pid_input;

	//printf("pid %ld\n", res.pid);

	qb_ipcs_event_send(c, &proto, sizeof(proto));

	//qb_ipcs_response_send(c, &res, sizeof(res));

	//printf("connect: %ld\n", (long)c);

	return scope.Close(Undefined());
}

Handle<Value> IpcConn::Resp(const v8::Arguments& args) {
	HandleScope scope;

	struct my_proto proto;
	memset(&proto, 0, sizeof(proto));

	size_t buffersize = node::Buffer::Length(args[0]->ToObject());
	char* bufferdata = node::Buffer::Data(args[0]->ToObject());

	proto.len = buffersize;
		
	memcpy(proto.message, bufferdata, buffersize);

	Local<Object> self = args.Holder();
	Local<External> wrap = Local<External>::Cast(self->GetInternalField(0));
	Local<External> wrap1 = Local<External>::Cast(self->GetInternalField(1));

	qb_ipcs_connection_t * c = (qb_ipcs_connection_t *)(wrap->Value());

	int64_t* pid = (int64_t*)(wrap1->Value());

	proto.pid = *pid;

	//printf("pid %ld\n", res.pid);

	qb_ipcs_event_send(c, &proto, sizeof(proto));

	//qb_ipcs_response_send(c, &res, sizeof(res));

	//printf("connect: %ld\n", (long)c);

	return scope.Close(Undefined());
}
