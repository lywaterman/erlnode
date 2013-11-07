#ifndef IPCCONN_H
#define IPCCONN_H

#include <node.h>

extern "C" {
	#include <qb/qbdefs.h>
	#include <qb/qbutil.h>
	#include <qb/qblog.h>
	#include <qb/qbloop.h>
	#include <qb/qbipcs.h> 
}

struct my_req {
	struct qb_ipc_request_header hdr;
	int64_t pid;
	int len;
	char message[256];
};

struct my_res {
	struct qb_ipc_response_header hdr;
	int64_t pid;
	int len;
	int is_req;
	int req_pid;
	char message[256];
};

using namespace v8;

class IpcConn: public node::ObjectWrap {
	public:
		static void Init(Handle<Object> exports);
		static Handle<Object> NewInstance();	

	private:
		explicit IpcConn();
		~IpcConn();

		static v8::Handle<v8::Value> New(const Arguments& args);
		static v8::Handle<v8::Value> Send(const v8::Arguments& args); 			

		static v8::Persistent<v8::Function> constructor;	
	
};


#endif
