#include "erlnode.hpp"

#include <dlfcn.h>
#include <unistd.h>

#include <assert.h>

namespace erlnode {

ERL_NIF_TERM response_to_erlbinary(ErlNifEnv* env, my_res & response) {
	ErlNifBinary binary;
    if (!enif_alloc_binary(response.len, &binary)) {
        throw errors::enomem();
    }
    memcpy(binary.data, response.message, response.len);
    return enif_make_binary(env, &binary);
}

void send_result_req(vm_t & vm, std::string const& type, my_res & response)
{
	ErlNifEnv* env = enif_alloc_env();
	
	ERL_NIF_TERM result_array[2];

    result_array[0] = enif_make_atom(env, "ok");
    result_array[1] = response_to_erlbinary(env, response);

	ERL_NIF_TERM result = enif_make_tuple_from_array(env, result_array, 2);

	ErlNifPid caller;
	caller.pid = response.req_pid;

	//printf("pid %ld\n", response.pid);

	ERL_NIF_TERM packet[3];
    packet[0] = enif_make_atom(env, type.c_str());
    packet[1] = result;
	packet[2] = enif_make_pid(env, &caller);

	ERL_NIF_TERM tuple = enif_make_tuple_from_array(env, packet, 3);

    enif_send(NULL, &caller, env, tuple);

	enif_free_env(env);
}

void send_result_res(vm_t & vm, std::string const& type, my_res & response)
{
	ErlNifEnv* env = enif_alloc_env();
	
	ERL_NIF_TERM result_array[2];

    result_array[0] = enif_make_atom(env, "ok");
    result_array[1] = response_to_erlbinary(env, response);

	ERL_NIF_TERM result = enif_make_tuple_from_array(env, result_array, 2);

	ErlNifPid caller;
	caller.pid = response.pid;

	//printf("pid %ld\n", response.pid);

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
	stop_thread = false;
	conn = NULL;
}

vm_t::~vm_t()
{
//     enif_fprintf(stderr, "*** destruct the vm\n");
}

/////////////////////////////////////////////////////////////////////////////

vm_t* vm_t::create(ErlNifResourceType* res_type, ErlNifPid const& pid)
{
    //enif_fprintf(stdout, "vm_t create------------------------------------------------------------------\n");
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

			//sleep(0);
			if (this->conn != NULL) {
				struct my_res res;
				memset(&res, 0, sizeof(res));

				qb_ipcc_event_recv(this->conn, &res, sizeof(res), 0);
				
				if (res.pid > 0) {
					//printf("%s\n", res.message);
					if (res.is_req) {
						send_result_req(*this, "nodeerl_request", res);
					} else {	
						send_result_res(*this, "nodeerl_response", res);
					}
				}
			}

            //perform_task<call_handler>(*this);
			if (this->stop_thread) {
				break;
			} 
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
	stop_thread = true;
    enif_thread_join(tid_, NULL);
};

void* vm_t::thread_run(void * vm)
{
    static_cast<vm_t*>(vm)->run();
    return 0;
}

/////////////////////////////////////////////////////////////////////////////

}
