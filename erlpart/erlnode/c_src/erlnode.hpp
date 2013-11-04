#pragma once

extern "C" {
    #include <qb/qbdefs.h>
    #include <qb/qbutil.h>
    #include <qb/qbipcc.h>
    #include <qb/qblog.h>
}

#include "erl_nif.h"

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
	char message[256];
};

namespace erlnode {

class vm_t
{
private:
    vm_t(ErlNifPid const& pid);
    ~vm_t();

    void run();
    void stop();

    static void* thread_run(void * vm);

public :

    ErlNifPid erl_pid() const { return pid_; }

    static void destroy(ErlNifEnv* env, void* obj);
    static vm_t* create(ErlNifResourceType* res_type, ErlNifPid const& pid);

    qb_ipcc_connection_t 	 *conn;
private :
    ErlNifPid               	 pid_;
    ErlNifTid                    tid_;
};

}
