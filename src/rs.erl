-module(rs).
-behavior(gen_server).

-export([start/1, start/2, stop/1, store/2, retrieve/2, configure/2, callbacks/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,  terminate/2]).

-record(state, {mtree=merkle_lazy:empty(), store=orddict:new(), callee, beb, interval=10000, scheduled=false}).

start(Callee) ->
	gen_server:start_link(?MODULE, [Callee] ,[]).
start(Callee,UpdInterval) ->
	gen_server:start_link(?MODULE, [Callee, UpdInterval], []).

store(Pid, Value) ->
	gen_server:call(Pid, {rsStore, Value}).

retrieve(Pid, Key) ->
	gen_server:call(Pid, {rsRetrieve, Key}).

callbacks(Pid) ->
	gen_server:call(Pid, callbacks).

configure(Pid, Refs) ->
	gen_server:call(Pid, {reconfigure, Refs}).

stop(Pid) ->
	gen_server:cast(Pid, stop).

%%GEN_SERVER

init([Callee]) ->
	{ok, BeB} = beb:start(self()),
	{ok, #state{beb = BeB, callee = Callee}};
init([Callee,T]) ->
	{ok, BeB} = beb:start(self()),
	{ok, #state{beb = BeB, callee = Callee, interval=T}}.

handle_call(callbacks, _From, C=#state{}) ->
	{reply, {self(),C#state.beb}, C};	
handle_call({reconfigure, Refs}, _From, C=#state{}) ->
	beb:configure(C#state.beb, Refs),
	case not C#state.scheduled of
		true -> erlang:send_after(C#state.interval, self(), sync)
	end,
	{reply, ok, C#state{scheduled=true}};
handle_call({rsStore, Val}, _From, C=#state{}) ->
	HashKey = crypto:sha(term_to_binary(Val)),
	C#state.callee ! {rsValue, HashKey, Val},
	{reply, ok, C#state{mtree=merkle_lazy:add(HashKey,C#state.mtree), store=orddict:store(HashKey, Val, C#state.store)}};
handle_call({rsRetrieve, Key}, _From, C=#state{}) ->
	C#state.callee ! {rsValue,Key,orddict:fetch(Key, C#state.store)},
	{reply, ok, C}.

handle_cast({rs_TREE, From, Mtree}, C=#state{}) ->
	% io:format("~n received the following Mtree ~p", [Mtree]),
	{ReqVals, SendVals} = merkle_lazy:diff(C#state.mtree, Mtree),
	% io:format("~n reqvals:~p , sendvals:~p", [ReqVals,SendVals]),
	lists:foreach(fun(Rval)-> gen_server:cast(From, {rs_RVALUE, self(), Rval}) end, ReqVals),
	lists:foreach(fun(Key)-> gen_server:cast(From, {rs_VALUE, self(), Key, orddict:fetch(Key, C#state.store)}) end, SendVals),
	{noreply,C};
handle_cast({rs_RVALUE, From, Key}, C=#state{}) ->
	gen_server:cast(From, {rs_VALUE, self(), Key, orddict:fetch(Key,C#state.store)}),
	{noreply, C};
handle_cast({rs_VALUE, _From, Key, Val}, C=#state{}) ->
	C#state.callee ! {rsValue, Key, Val},
	{noreply, C#state{store=orddict:store(Key,Val,C#state.store), mtree=merkle_lazy:add(Key, C#state.mtree)}};
handle_cast(stop, C=#state{}) ->
	beb:stop(C#state.beb),
	{stop, normal, C}.

handle_info(sync, C=#state{}) ->
	beb:broadcast(C#state.beb, {rs_ROOT, self(), merkle_lazy:roothash(C#state.mtree)}),
	erlang:send_after(C#state.interval, self(), sync),
	{noreply, C};
handle_info({bebdeliver, {rs_ROOT, From, Rhash}}, C=#state{}) ->
	case Rhash /= merkle_lazy:roothash(C#state.mtree) of
		true -> gen_server:cast(From, {rs_TREE, self(), C#state.mtree}),
				{noreply,C}; 
		false ->
				{noreply,C}
	end.

terminate(_Reason, _State) ->
	ok.







