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
