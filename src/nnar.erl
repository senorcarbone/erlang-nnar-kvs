-module(nnar).
-beheavior(gen_server).

-export([start/2, stop/1, read/1, write/2, configure/2, callbacks/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,  terminate/2, completionCheck/1]).

-record(state, {register={0,0,none}, callee, beb, correct=gb_sets:empty(), writeset=gb_sets:empty(), readval=none, reading=false, fdsupport=false}).


%%PUBLIC API

start(Callee, FD_support) ->
	gen_server:start_link(?MODULE, [Callee, FD_support], []).

read(Pid) ->
	gen_server:call(Pid, nnar_read).

write(Pid, Val) ->
	gen_server:call(Pid, {nnar_write, Val}).

configure(Pid, Refs) ->
	gen_server:call(Pid, {reconfigure, Refs}).

callbacks(Pid) ->
	gen_server:call(Pid, callbacks).

stop(Pid) ->
	gen_server:cast(Pid, stop).

%% GEN_SERVER 

init([Callee, FD_support]) ->
	{ok, BeB} = beb:start(self()),
	{ok, #state{callee=Callee, beb=BeB, fdsupport=FD_support}}.

handle_call(nnar_read, _From, C=#state{}) ->
	beb:broadcast(C#state.beb, {reg_write, self(), C#state.register}),
	{_, _, Val} = C#state.register,
	{reply, ok, C#state{reading=true, readval=Val}};
handle_call({nnar_write, Val}, _From, C=#state{}) ->
	{Ts,_,_} = C#state.register,
	beb:broadcast(C#state.beb, {reg_write, self(), {Ts+1, self(), Val}}),
	{reply, ok, C};
handle_call(callbacks, _From, C=#state{}) ->
	{reply, {self(),C#state.beb}, C};	
handle_call({reconfigure, Refs}, _From, C=#state{}) ->
	Beb_conf = lists:foldl(fun({_, BEB_NODE},AccIn)-> [BEB_NODE | AccIn] end, [], Refs),
	beb:configure(C#state.beb, Beb_conf),
	Nnar_conf = lists:foldl(fun({NNAR_NODE, _},AccIn)-> [NNAR_NODE | AccIn] end, [], Refs),
	monitor_support(C#state.fdsupport, Nnar_conf),
	{reply, ok, C#state{correct = gb_sets:from_list(Nnar_conf)}}.

handle_cast({nnar_ack, Pid}, C=#state{}) ->
	{noreply, completionCheck(C#state{writeset = gb_sets:add(Pid, C#state.writeset)})};
handle_cast(stop, C=#state{}) ->
	beb:stop(C#state.beb),
	{stop, normal, C}.

handle_info({bebdeliver, {reg_write, Pid, Reg}}, C=#state{}) when Reg > C#state.register ->
	gen_server:cast(Pid, {nnar_ack, self()}),
	{noreply, C#state{register=Reg}};
handle_info({bebdeliver, {reg_write, Pid, _Reg}}, C=#state{}) ->
	gen_server:cast(Pid, {nnar_ack, self()}),
	{noreply,C};
handle_info({'DOWN', _, process, Pid, _}, C=#state{}) ->
	{noreply, completionCheck(C#state{correct=gb_sets:delete_any(Pid,C#state.correct)})}.

terminate(_Reason, _State) ->
	ok.

%% PRIVATE FUNCTIONS

monitor_support(false, _) -> ok;
monitor_support(true, Processes) ->
	lists:foreach(fun(Pid) -> erlang:monitor(process, Pid) end, Processes),
	ok.

completionCheck(C=#state{}) ->
		case gb_sets:is_subset(C#state.correct, C#state.writeset) of
			true -> 
				case C#state.reading of
					true -> 
						C#state.callee ! {nnar_doneread, self(), C#state.readval},
						C#state{reading=false, writeset=gb_sets:empty()};
					false ->
						C#state.callee ! {nnar_donewrite, self()},
						C#state{writeset=gb_sets:empty()}
				end;
			false -> C
		end.


