-module(kvs).
-behavior(gen_server).

-export([start/1, start/2, stop/1, put/3, get/2, configure/3, callbacks/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,  terminate/2, addItemToStoreList/3]).

-record(state, {callee, beb, rs, ts=0, store=orddict:new(), writeSet=orddict:new(), 
	readSet=orddict:new(), writeQuorum=0, readQuorum=0, pendingRead=orddict:new(), 
	pendingWrite=orddict:new(),pendingGetReply=orddict:new(), pendingPutReply=orddict:new()}).

start(Callee) ->
	gen_server:start_link(?MODULE, [Callee], []).

start(Callee, Interv) ->
	gen_server:start_link(?MODULE, [Callee, Interv], []).

stop(Pid) ->
	gen_server:cast(Pid, stop).

put(Pid, Key, Value) ->
	gen_server:call(Pid, {kvs_put, Key, Value}).

get(Pid, Key) ->
	gen_server:call(Pid, {kvs_get, Key}).

callbacks(Pid) ->
	gen_server:call(Pid, callbacks).

configure(Pid, Instances, Quorums) ->
	gen_server:call(Pid, {reconfigure, Instances, Quorums}).

%%GEN_SERVER

init([Callee]) ->
	init([Callee, 8000]);
init([Callee, Interv]) ->
	{ok, BeB} = beb:start(self()),
	{ok, ReplStore} = rs:start(self(), Interv),
	{ok, #state{beb = BeB, rs=ReplStore, callee = Callee}}.

handle_call(callbacks, _From, C=#state{}) ->
	{reply, {self(), C#state.beb, rs:callbacks(C#state.rs)}, C};	
handle_call({reconfigure, Instances, {WriteQuorum, ReadQuorum}}, _From, C=#state{}) ->
	BebConfig = lists:foldl(fun({_,BepInst,_}, Acc)-> [BepInst|Acc] end,[], Instances),
	RsConfig = lists:foldl(fun({_,_,NrInst}, Acc)-> [NrInst|Acc]end, [], Instances),
	beb:configure(C#state.beb, BebConfig),
	RsBebConfig = lists:foldl(fun({_,BEB},Acc)-> [BEB|Acc] end, [], RsConfig),
	rs:configure(C#state.rs, RsBebConfig),
	{reply, ok, C#state{writeQuorum=WriteQuorum, readQuorum=ReadQuorum}};
handle_call({kvs_put, Key, Val}, _From, C=#state{}) ->
	beb:broadcast(C#state.beb, {put, {self(), C#state.ts, Key, Val}}),
	{reply, ok, C#state{writeSet=orddict:store(Key,[],C#state.writeSet), ts=C#state.ts+1, pendingPutReply=orddict:store(Key, true, C#state.pendingPutReply)}};
handle_call({kvs_get, Key}, _From, C=#state{}) ->
	beb:broadcast(C#state.beb, {get, {self(), Key}}),
	{reply, ok, C#state{readSet=orddict:store(Key,[],C#state.readSet), pendingGetReply=orddict:store(Key, true, C#state.pendingGetReply)}}.

handle_cast({put_reply, From, Key}, C=#state{}) ->
	% io:format("~n ~p got putreply from ~p for key ~p",[self(), From, Key ]),
	case orddict:is_key(Key, C#state.pendingPutReply) of
		true ->
			UpdWriteSet = addItemToStoreList(From, Key, C#state.writeSet),
			case length(orddict:fetch(Key,UpdWriteSet)) >= C#state.writeQuorum of
				true -> 
					C#state.callee ! {kvsputreply, Key},
					{noreply, C#state{writeSet = orddict:erase(Key, C#state.writeSet), pendingPutReply=orddict:store(Key, false, C#state.pendingPutReply)}};
				false ->
					{noreply, C#state{writeSet=UpdWriteSet}}
			end;
		false ->
			{noreply, C}
	end;
handle_cast({get_reply, From, {NodeId,ValueTS,Key,Value}}, C=#state{}) ->
	case orddict:is_key(Key,C#state.pendingGetReply) of
		true ->
			UpdReadSet = addItemToStoreList({From, {NodeId,ValueTS,Key,Value}}, Key, C#state.readSet),
			case length(orddict:fetch(Key,UpdReadSet)) >= C#state.readQuorum of
				true -> 
					{_,{_,_,_,Val}} = lists:max(orddict:fetch(Key,C#state.readSet)),
					C#state.callee ! {kvsgetreply, Key, Val},
					{noreply, C#state{readSet=orddict:erase(Key, C#state.readSet), pendingGetReply=orddict:store(Key, false, C#state.pendingGetReply)}};
				false ->
					{noreply, C#state{readSet=UpdReadSet}}
			end;
		false -> 
			{noreply,C}
	end;
handle_cast(stop, C=#state{}) ->
	beb:stop(C#state.beb),
	rs:stop(C#state.rs),
	{stop, normal, C}.


handle_info({rsValue, Hash, Data}, C=#state{}) ->
	% io:format("~n ~p got rsValue for hash ~p and Data: ~p ",[self(), Hash, Data]),
	{NodeId, ValueTS, Key, _Value} = Data,
	UpdC = 
	case orddict:is_key(Key,C#state.store) of
		true ->
			{StoreTs, _} = orddict:fetch(Key, C#state.store),
			NewStore =
			case {StoreTs, self()} < {ValueTS, NodeId} of
				true ->
					orddict:store(Key, {ValueTS, Hash}, C#state.store);
				false->
					orddict:store(Key, {StoreTs, Hash}, C#state.store)
			end,
			C#state{ts=lists:max([ValueTS,C#state.ts]), store=NewStore};
		false ->
			C#state{ts=lists:max([ValueTS,C#state.ts]), store=orddict:store(Key,{ValueTS, Hash},C#state.store)}
	end,

	case orddict:is_key(Key, UpdC#state.pendingRead) of
		true ->
			lists:foreach(fun(Node)-> gen_server:cast(Node,{get_reply, self(), Data}) end, 
			orddict:fetch(Key,UpdC#state.pendingRead));
		false -> ok
	end,
	ToRemove = lists:foldl(
		fun({LNode,LNodeID,LValueTS}, Acc)-> 
			gen_server:cast(LNode, {put_reply, self(), Key}),
			[{LNode,LNodeID,LValueTS}|Acc]
		end,[], orddict:fetch(Key,UpdC#state.pendingWrite)),
	PendingW = orddict:fetch(Key, UpdC#state.pendingWrite),
	UpdPendingWriteList = lists:foldl(fun(P, Acc)-> lists:delete(P,Acc) end, PendingW, ToRemove),
	UpdPendWrite = orddict:store(Key, UpdPendingWriteList ,C#state.pendingWrite),
	{noreply, UpdC#state{pendingRead=orddict:new(), pendingWrite=UpdPendWrite}};
handle_info({bebdeliver, {put, Data}}, C=#state{}) ->
	% io:format("~n ~p got put for : ~p",[self(), Data]),
	{NodeID, ValueTS, Key, _Value} = Data,
	% io:format("~n ~p updating pendingWrite to ~p",[self(), addItemToStoreList({NodeID, NodeID, ValueTS},Key,C#state.pendingWrite)]),
	rs:store(C#state.rs, Data),
	{noreply, C#state{pendingWrite=addItemToStoreList({NodeID, NodeID, ValueTS},Key,C#state.pendingWrite)}};
handle_info({bebdeliver, {get, {Node, Key}}}, C=#state{})  ->
	case orddict:is_key(Key, C#state.store) of
	true ->
		{_,Hash} = orddict:fetch(Key,C#state.store),
		rs:retrieve(C#state.rs, Hash),
		{noreply, C#state{pendingRead=addItemToStoreList(Node,Key,C#state.pendingRead)}};
	false ->
		gen_server:cast(Node,{get_reply, self(), {self(), C#state.ts-1, Key, nil}}),
		{noreply, C}
	end.

terminate(_Reason, _State) ->	
	ok.

%%private functions
addItemToStoreList(Item, Key , Store) ->
	case(orddict:is_key(Key,Store)) of
		true ->
			FilteredSet = gb_sets:from_list([Item|orddict:fetch(Key,Store)]),
			orddict:store(Key, gb_sets:to_list(FilteredSet),Store);
		false ->
			orddict:store(Key, [Item], Store)
	end.






