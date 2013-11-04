#include "erlnode.hpp"
#include "errors.hpp"

#include <dlfcn.h>
#include <unistd.h>

namespace erlnode {

ERL_NIF_TERM response_to_erlbinary(ErlNifEnv* env, my_res & response) {
	ErlNifBinary binary;
    if (!enif_alloc_binary(response.len, &binary)) {
        throw errors::enomem();
    }
    memcpy(response.message, binary.data, response.len);
    return enif_make_binary(env, &binary);
}

void send_result_caller(vm_t & vm, std::string const& type, my_res & response)
{
	ErlNifEnv* env = enif_alloc_env();
	
	ERL_NIF_TERM result_array[2];

    result_array[0] = enif_make_atom(env, "ok");
    result_array[1] = response_to_erlbinary(env, response);

	ERL_NIF_TERM result = enif_make_tuple_from_array(env, &result, 2);

	ErlNifPid caller;
	caller.pid = response.pid;

	ERL_NIF_TERM packet[3];
    packet[0] = enif_make_atom(env, type.c_str());
    packet[1] = result;
	packet[2] = enif_make_pid(env, &caller);

	ERL_NIF_TERM tuple = enif_make_tuple_from_array(env, packet, 3);

    enif_send(NULL, &caller, env, tuple);

	enif_free_env(env);
}


vm_t::vm_t(ErlNifPid const& pid)
    : pid_(pid) 
{
	conn = NULL;
}

vm_t::~vm_t()
{
//     enif_fprintf(stderr, "*** destruct the vm\n");
}

/////////////////////////////////////////////////////////////////////////////

vm_t* vm_t::create(ErlNifResourceType* res_type, ErlNifPid const& pid)
{
    enif_fprintf(stdout, "vm_t create------------------------------------------------------------------\n");
    void * buf = enif_alloc_resource(res_type, sizeof(vm_t));
    // TODO: may leak, need to guard agaist
	//enif_release_resource
    vm_t* result = new (buf) vm_t(pid);

    if(enif_thread_create(NULL, &(result->tid_), vm_t::thread_run, result, NULL) != 0) {
		return NULL; 
    }

    return result;
}

void vm_t::destroy(ErlNifEnv* env, void* obj)
{
    static_cast<vm_t*>(obj)->stop();
    static_cast<vm_t*>(obj)->~vm_t();
}

void vm_t::run()
{
    try
    {
        for(;;)
        {
			//struct my_res res;
			//memset(&res, 0, sizeof(res));
			//int rc = qb_ipcc_recv(this->conn, &res, sizeof(res), -1);
			//printf("%s\n", res.message);
			printf("123123123123123\n");
			if (this->conn != NULL) {
				struct my_res res;
				memset(&res, 0, sizeof(res));

				int rc = qb_ipcc_event_recv(this->conn, &res, sizeof(res), -1);
				if (rc > 0) {
										
					send_result_caller(*this, "erlnode_response", res);

					printf("%s\n", res.message);
				}
			}

			//sleep(1000);
            //perform_task<call_handler>(*this);
		}
    }
    catch(std::exception & ex)
    {
        enif_fprintf(stderr, "*** exception in vm thread: %s\n", ex.what());
    }
    catch(...) {}
}

void vm_t::stop()
{
    enif_thread_join(tid_, NULL);
};

void* vm_t::thread_run(void * vm)
{
    static_cast<vm_t*>(vm)->run();
    return 0;
}

/////////////////////////////////////////////////////////////////////////////

}
