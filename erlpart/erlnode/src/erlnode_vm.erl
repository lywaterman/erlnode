-module(erlnode_vm).
-behaviour(gen_server).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% gen_server callbacks:
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% api:
-export([start_link/1]).
-export([send/3]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Public api:

start_link(Options) ->
    gen_server:start_link(?MODULE, Options, []).

send(Pid, Bin, Timeout) ->
	gen_server:call(Pid, {send, Bin, self()}, Timeout),
	receive_response_self().


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Private api:

-record(state, {vm}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

init(Options) ->
    {ok, VM} = erlnode_nif:start(self()),
    {ok, #state{vm=VM}}.

handle_call({send, Bin, Caller}, _, State=#state{vm=VM}) ->
    ok = erlnode_nif:send(VM, Bin, Caller),
    {reply, ok, State}.

handle_cast(_, State) ->
    {noreply, State}.

handle_info(_, State) ->
    {noreply, State}.

terminate(_, _) ->
    ok.

code_change(_, State, _) ->
    {ok, State}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

receive_response_self() ->
    receive
        {erlnode_response, Response, Caller} ->
            Response
	end.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

to_atom(Val) when is_atom(Val) -> Val;
to_atom(Val) when is_list(Val) -> list_to_atom(Val);
to_atom(Val) when is_binary(Val) -> list_to_atom(binary_to_list(Val)).

to_binary(Val) when is_binary(Val) -> Val;
to_binary(Val) when is_atom(Val) -> list_to_binary(atom_to_list(Val));
to_binary(Val) when is_list(Val) -> list_to_binary(Val).
