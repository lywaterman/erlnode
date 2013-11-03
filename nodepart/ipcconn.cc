#define BUILDING_NODE_EXTENSION
#include <node.h>
#include "ipcserver.h"

using namespace v8;

Persistent<Function> IpcServer::constructor;


IpcServer::IpcServer(qb_ipcs_connection_t * c): c_(c) {

}

IpcServer::~IpcServer() {

}

void IpcServer::Init(Handle<Object> exports) {
	// Prepare constructor template
	Local<FunctionTemplate> tpl = FunctionTemplate::New(New);
	tpl->SetClassName(String::NewSymbol("IpcServer"));
	tpl->InstanceTemplate()->SetInternalFieldCount(1);

	//Prototype
	tpl->PrototypeTemplate()->Set(String::NewSymbol("send"),
		FunctionTemplate::New(Send)->GetFunction());

	constructor = Persistent<Function>::New(tpl->GetFunction());
	exports->Set(String::NewSymbol("IpcServer"), constructor);
}

Handle<Value> IpcServer::New(const Arguments& args) {
	HandleScope scope;

	if (args.IsConstructCall()) {
		int value = (int)(args[0]->IsUndefined() ? 0 : args[0]->NumberValue());
		
		IpcServer *obj = new IpcServer(value);
		obj->Wrap(args.This());
		return args.This();
	} else {
		const int argc = 1;
		Local<Value> argv[argc] = {args[0]};
		return scope.Close(constructor->NewInstance(argc, argv));
	}
}


Handle<Value> IpcServer::Send(const v8::Arguments& args) {
	HandleScope scope;

	IpcServer* obj = ObjectWrap::Unwrap<IpcServer>(args.This());

}
