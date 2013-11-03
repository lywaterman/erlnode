#include <node.h>
#include "ipcconn.h"

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
	tpl->InstanceTemplate()->SetInternalFieldCount(1);

	//Prototype
	tpl->PrototypeTemplate()->Set(String::NewSymbol("send"),
		FunctionTemplate::New(Send)->GetFunction());

	constructor = Persistent<Function>::New(tpl->GetFunction());
	exports->Set(String::NewSymbol("IpcConn"), constructor);
}

Handle<Value> IpcConn::New(const Arguments& args) {
	HandleScope scope;

	if (args.IsConstructCall()) {
		printf("construct call\n");
		
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

	const unsigned argc = 0;
	Local<Value> argv[argc] = { };

	return scope.Close(constructor->NewInstance());
}


Handle<Value> IpcConn::Send(const v8::Arguments& args) {
	HandleScope scope;

	IpcConn* obj = ObjectWrap::Unwrap<IpcConn>(args.This());

	Local<Object> self = args.Holder();
	Local<External> wrap = Local<External>::Cast(self->GetInternalField(0));

	qb_ipcs_connection_t * c = (qb_ipcs_connection_t *)(wrap->Value());

}
