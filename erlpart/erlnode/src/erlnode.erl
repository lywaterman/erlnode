-module(erlnode).

-export([start/0, stop/0]).
-export([start_vm/0, start_vm/1, stop_vm/1]).

-export([send/2, send/3]).

-export([test/1]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

start() ->
    start(?MODULE).

start(App) ->
    start_ok(App, application:start(App, permanent)).

stop() ->
    application:stop(?MODULE).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

start_ok(_, ok) ->
    ok;

start_ok(_, {error, {already_started, _App}}) ->
    ok;

start_ok(App, {error, {not_started, Dep}}) when App =/= Dep ->
    ok = start(Dep),
    start(App);

start_ok(App, {error, Reason}) ->
    erlang:error({app_start_failed, App, Reason}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

start_vm() ->
    start_vm([]).

start_vm(Options) ->
    erlnode_sup:start_child([Options]).

stop_vm(Pid) ->
    erlnode_sup:stop_child(Pid).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

send(Pid, Bin) ->
    send(Pid, Bin, infinity).

send(Pid, Bin, Timeout) ->
    erlnode_vm:call(Pid, Bin, Timeout).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

test(Args) ->
    io:format("Callback hit the erlang! Args = ~p~n", [Args]),
    {ok, {tha_tuple, <<"binary">>, [{<<"key">>,<<"value">>}]}, []}.
