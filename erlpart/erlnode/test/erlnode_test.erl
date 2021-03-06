-module(erlnode_test).
-include_lib("eunit/include/eunit.hrl").

the_test_() ->
    {foreach,
        fun setup/0,
        fun teardown/1,
        [
            {"Starting/Stopping the VM",
                fun() -> ok end
            },
            {"Erlang -> Lua type mapping",
                fun() ->
			{ok, Res} = erlnode:call(vm, <<"./test">>, <<"test_fun">>, [<<"121123">>]),
			?assertMatch(<<"121123">>, bert:decode(Res))
                end
            }
            %%{"Lua -> Erlang type mapping",
            %%    fun() ->
			%%		erlnode:send(vm, bert:encode([<<"call">>, <<"async">>, <<"do">>,<<"121123">>]))
            %%    end
            %%}
        ]
    }.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

setup() ->
    error_logger:tty(false),
    application:start(erlnode),
    {ok, Res} = erlnode:start_vm(),
    register(vm, Res).

teardown(_) ->
    ok = erlnode:stop_vm(whereis(vm)),
    application:stop(erlnode).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
