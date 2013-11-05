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
	tpl->PrototypeTemplate()->Set(String::NewSymbol("send"),
		FunctionTemplate::New(Send)->GetFunction());

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


Handle<Value> IpcConn::Send(const v8::Arguments& args) {
	HandleScope scope;

	struct my_res res;
	memset(&res, 0, sizeof(res));

	size_t buffersize = node::Buffer::Length(args[0]->ToObject());
	char* bufferdata = node::Buffer::Data(args[0]->ToObject());

	res.hdr.size = sizeof(struct my_res);
	res.len = buffersize;
		
	memcpy(res.message, bufferdata, buffersize);

	Local<Object> self = args.Holder();
	Local<External> wrap = Local<External>::Cast(self->GetInternalField(0));
	Local<External> wrap1 = Local<External>::Cast(self->GetInternalField(1));

	qb_ipcs_connection_t * c = (qb_ipcs_connection_t *)(wrap->Value());

	int64_t* pid = (int64_t*)(wrap1->Value());

	res.pid = *pid;

	//printf("pid %ld\n", res.pid);

	qb_ipcs_event_send(c, &res, sizeof(res));

	//qb_ipcs_response_send(c, &res, sizeof(res));

	//printf("connect: %ld\n", (long)c);

	return scope.Close(Undefined());
}
