-module(rstest).
-export([testRS/1]).
-include_lib("eunit/include/eunit.hrl").

testRS(NodeNum) ->
	Instances = getRSInstances(NodeNum),
	BebInst = lists:foldl(fun({_,BEB},Acc)-> [BEB|Acc] end, [], Instances),
	?assertEqual(NodeNum, length(Instances)),
	configureRS(Instances, BebInst),
	testReplicas(Instances),
	ok.

getRSInstances(Num) ->
	getRSInstances([],Num).

configureRS(Processes, BebInst) ->
	lists:foreach(fun({Inst,_}) -> rs:configure(Inst, BebInst) end, Processes).

getRSInstances(Instances,0) ->
	Instances;
getRSInstances(Instances, N) ->
	{ok, Pid} = rs:start(self(),3000),
	Instance = rs:callbacks(Pid),
	getRSInstances([Instance|Instances], N-1).

testReplicas([ Head | Rest]) ->
	{Inst,_} = Head,
	Val = 123123,
	rs:store(Inst, Val),
	receive
		{rsValue, Key, Val} ->
			timer:sleep(timer:seconds(5)),
			checkRead(Rest, Key, Val),
			ok
		after 5000 ->
			?assert(false)
	end.
	

checkRead([], _, _) -> ok;
checkRead([ {Inst,_} | Instances], Key, MatchVal) ->
	rs:retrieve(Inst, Key),
	receive
		{rsValue, _, Val} ->
			?assertEqual(MatchVal, Val)
		after 5000 ->
			?assert(false)
	end,
	checkRead(Instances, Key,MatchVal).

