-module(kvstest).
-export([testKVS/1]).
-include_lib("eunit/include/eunit.hrl").

testKVS(NodeNum) ->
	Configuration = getKVSInstances(NodeNum),
	?assertEqual(NodeNum, length(Configuration)),
	% io:format("configuration is ~p",[Configuration]),
	configureKVS(Configuration),
	testStore(Configuration),
	ok.


configureKVS(Configuration) ->
	Quorums = length(Configuration),
	{WriteQuorum, ReadQuorum} = {(Quorums div 2)+1, Quorums div 2},
	lists:foreach(fun({Inst,_,_}) -> kvs:configure(Inst, Configuration, {WriteQuorum, ReadQuorum} ) end, Configuration).

getKVSInstances(Num) ->
	getKVSInstances([],Num).

getKVSInstances(Instances,0) ->
	Instances;
getKVSInstances(Instances, N) ->
	{ok, Pid} = kvs:start(self(),3000),
	Instance = kvs:callbacks(Pid),
	getKVSInstances([Instance|Instances], N-1).

testStore([ Head | Rest	]) ->
	{Inst,_,_} = Head,
	Val = 123123,
	Key = key1,
	kvs:put(Inst, Key, Val),
	receive
		{kvsputreply, Key} ->
			timer:sleep(timer:seconds(5)),
			checkRead(Rest, Key, Val),
			ok
		after 5000 ->
			?assert(false)
	end.
	

checkRead([], _, _) -> ok;
checkRead([ {Inst,_,_} | Instances], Key, MatchVal) ->
	kvs:get(Inst, Key),
	receive
		{kvsgetreply, Key, Val} ->
			?assertEqual(MatchVal, Val)
		after 5000 ->
			?assert(false)
	end,
	checkRead(Instances, Key,MatchVal).

