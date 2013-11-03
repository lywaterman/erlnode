extern "C" {
	#include <qb/qbdefs.h>
	#include <qb/qbutil.h>
	#include <qb/qblog.h>
	#include <qb/qbloop.h>
	#include <qb/qbipcs.h> 
}

#include <node.h>
#include <v8.h>
#include <node_buffer.h>
#include <string>

#include <stdio.h>

using namespace v8;

struct my_req {
	struct qb_ipc_request_header hdr;
	int len;
	char message[256];
};


static Local<Function> cb_;
static qb_loop_t *bms_loop;
static qb_ipcs_service_t *s1;

static int32_t
s1_connection_accept_fn(qb_ipcs_connection_t * c, uid_t uid, gid_t gid)
{
	return 0;
}

static void
s1_connection_created_fn(qb_ipcs_connection_t * c)
{
	struct qb_ipcs_stats srv_stats;
	
	qb_ipcs_stats_get(s1, &srv_stats, QB_FALSE);
	qb_log(LOG_INFO, "Connection created (active:%d, closed:%d)",
		srv_stats.active_connections, srv_stats.closed_connections);
}

static void
s1_connection_destroyed_fn(qb_ipcs_connection_t *c)
{
	qb_log(LOG_INFO, "Connection about to be freed");
}

static int32_t
s1_connection_closed_fn(qb_ipcs_connection_t *c)
{
	return 0;
}

static int32_t
s1_msg_process_fn(qb_ipcs_connection_t * c, void *data, size_t size)
{
	struct qb_ipc_request_header *hdr;
	struct my_req *req_pt;	

	hdr = (struct qb_ipc_request_header *)data;

	req_pt = (struct my_req *)data;

	printf("%d\n", req_pt->len);
	printf("%s\n", req_pt->message);

	node::Buffer *buffer = node::Buffer::New(req_pt->len);

	memcpy(node::Buffer::Data(buffer), req_pt->message, req_pt->len);

	const unsigned argc = 2;	
	
	Local<Value> argv[argc]	= {Local<Value>::New(String::New("data")),
				   Local<Value>::New(buffer->handle_)};

	cb_->Call(Context::GetCurrent()->Global(), argc, argv);

	return 0;
}

static int32_t
my_job_add(enum qb_loop_priority p, void *data, qb_loop_job_dispatch_fn fn)
{
	return qb_loop_job_add(bms_loop, p, data, fn);
}

static int32_t
my_dispatch_add(enum qb_loop_priority p, int32_t fd, 
		int32_t evts, void *data, qb_ipcs_dispatch_fn_t fn)
{
	return qb_loop_poll_add(bms_loop, p, fd, evts, data, fn);
}

static int32_t
my_dispatch_mod(enum qb_loop_priority p, int32_t fd, int32_t evts,
		void *data, qb_ipcs_dispatch_fn_t fn)
{
	return qb_loop_poll_mod(bms_loop, p, fd, evts, data, fn);
}

static int32_t
my_dispatch_del(int32_t fd)
{
	return qb_loop_poll_del(bms_loop, fd);
}

Handle<Value> IpcServer_Listen(const Arguments& args) {
	HandleScope scope;

	std::string path = std::string(*v8::String::AsciiValue(args[0]));

	qb_ipc_type type = (qb_ipc_type)args[1]->NumberValue();

	cb_ = Local<Function>::Cast(args[2]);

	struct qb_ipcs_service_handlers sh; 
        sh.connection_accept = s1_connection_accept_fn;
        sh.connection_created = s1_connection_created_fn;
        sh.msg_process = s1_msg_process_fn;
        sh.connection_destroyed = s1_connection_destroyed_fn;
        sh.connection_closed = s1_connection_closed_fn;

	struct qb_ipcs_poll_handlers ph;
        ph.job_add = my_job_add;
       	ph.dispatch_add = my_dispatch_add;
       	ph.dispatch_mod = my_dispatch_mod;
       	ph.dispatch_del = my_dispatch_del;

	s1 = qb_ipcs_create(path.c_str(), 0, type, &sh);
	bms_loop = qb_loop_create();
	qb_ipcs_poll_handlers_set(s1, &ph);
	
	int rc = qb_ipcs_run(s1);
	
	qb_loop_run(bms_loop);


	return scope.Close(Undefined());
}


void init(Handle<Object> exports) {
	exports->Set(String::NewSymbol("ipcserver_listen"),
		FunctionTemplate::New(IpcServer_Listen)->GetFunction());
}

NODE_MODULE(nodeipc, init)
