#include <stdio.h>
#include "erlnode.hpp"

extern "C" {
    #include <qb/qbdefs.h>
    #include <qb/qbutil.h>
    #include <qb/qbipcc.h>
    #include <qb/qblog.h>
}

#define ONE_MEG 1048576
#define MAX_MSG_SIZE ONE_MEG

/////////////////////////////////////////////////////////////////////////////

static struct {
    ERL_NIF_TERM ok;
    ERL_NIF_TERM error;
    ERL_NIF_TERM enomem;
    ERL_NIF_TERM invalid_args;
    ERL_NIF_TERM invalid_type;
    ERL_NIF_TERM not_implemented;
} atoms;

/////////////////////////////////////////////////////////////////////////////

static ErlNifResourceType * res_type = 0;

static int init(ErlNifEnv *env, void **priv, ERL_NIF_TERM load_info)
{
    atoms.ok                = enif_make_atom(env, "ok");
    atoms.error             = enif_make_atom(env, "error");
    atoms.enomem            = enif_make_atom(env, "enomem");
    atoms.invalid_args      = enif_make_atom(env, "invalid_args");
    atoms.invalid_type      = enif_make_atom(env, "invalid_type");
    atoms.not_implemented   = enif_make_atom(env, "not_implemented");

    res_type = enif_open_resource_type(
        env, "erlnode", "erlnode_vm", erlnode::vm_t::destroy,
        static_cast<ErlNifResourceFlags>(ERL_NIF_RT_CREATE | ERL_NIF_RT_TAKEOVER), NULL
    );

    return (!!res_type) ? 0 : -1;
}

/////////////////////////////////////////////////////////////////////////////

static ERL_NIF_TERM start(ErlNifEnv * env, int argc, const ERL_NIF_TERM argv[])
{
    try
    {
        if (argc < 1)
        {
            return enif_make_badarg(env);
        }

	ErlNifPid pid;
	if (!enif_get_local_pid(env, argv[0], &pid)) {
        	throw errors::invalid_type("invalid_pid");
    	}

        erlnode::vm_t* vm = erlnode::vm_t::create(res_type, pid);

        ERL_NIF_TERM result = enif_make_resource(env, vm);

	vm->conn = qb_ipcc_connect("myipcserver", MAX_MSG_SIZE);

        return enif_make_tuple2(env, atoms.ok, result);
    }
    catch( std::exception & ex )
    {
        return enif_make_tuple2(env, atoms.error, enif_make_atom(env, ex.what()));
    }
}

static ERL_NIF_TERM send(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    try
    {
        if (argc != 3)
        {
            return enif_make_badarg(env);
        }

        erlnode::vm_t * vm = NULL;
        if(!enif_get_resource(env, argv[0], res_type, reinterpret_cast<void**>(&vm)))
        {
            return enif_make_badarg(env);
        }
		ErlNifBinary binary;
    	if (!enif_inspect_binary(env, argv[1], &binary)) {
        	throw errors::invalid_type("invalid_binary");
    	}

		ErlNifPid pid;
    	if (!enif_get_local_pid(env, argv[2], &pid)) {
    	    throw errors::invalid_type("invalid_pid");
    	}	

	struct my_req req;
	memset(&req, 0, sizeof(req));

	memcpy(req.message, binary.data, binary.size);
	req.len = binary.size;
	req.pid = (int64_t)pid.pid;

	printf("pid %ld\n", req.pid);

	req.hdr.id = QB_IPC_MSG_USER_START + 3;
	req.hdr.size = sizeof(struct my_req);

	qb_ipcc_send(vm->conn, &req, req.hdr.size);	

	// struct my_res res;
	//		
	//memset(&res, 0, sizeof(res));
	//int rc = qb_ipcc_recv(vm->conn, &res, sizeof(res), -1);
	//printf("%s\n", res.message);

        return atoms.ok;
    }
    catch( std::exception & ex )
    {
        return enif_make_tuple2(env, atoms.error, enif_make_atom(env, ex.what()));
    }
}

/////////////////////////////////////////////////////////////////////////////

static ErlNifFunc nif_funcs[] = {
    {"start", 1, start},
    {"send", 3, send}
};

ERL_NIF_INIT(erlnode_nif, nif_funcs, &init, NULL, NULL, NULL)

/////////////////////////////////////////////////////////////////////////////

