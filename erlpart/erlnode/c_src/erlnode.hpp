#pragma once

#include "errors.hpp"
extern "C" {
    #include <qb/qbdefs.h>
    #include <qb/qbutil.h>
    #include <qb/qbipcc.h>
    #include <qb/qblog.h>
}

#include "erl_nif.h"

struct my_proto {
	int64_t pid;
	int nodeerl_req;
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
	bool stop_thread;
    ErlNifPid erl_pid() const { return pid_; }

    static void destroy(ErlNifEnv* env, void* obj);
    static vm_t* create(ErlNifResourceType* res_type, ErlNifPid const& pid);

    qb_ipcc_connection_t 	 *conn;
private :
    ErlNifPid               	 pid_;
    ErlNifTid                    tid_;
};

}
