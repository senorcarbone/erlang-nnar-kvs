-module(nnartest).
-export([testNNAR/1]).
-include_lib("eunit/include/eunit.hrl").

testNNAR(NodeNum) ->
	Instances = getNnarInstances(NodeNum),
	?assertEqual(NodeNum, length(Instances)),
	configureNNAR(Instances),
	testWrite(Instances),
	ok.

getNnarInstances(Num) ->
	getNnarInstances([],Num).

configureNNAR(Processes) ->
	lists:foreach(fun({Inst,_}) -> beb:configure(Inst, Processes) end, Processes).
¡
getNnarInstances(Instances,0) ->
	Instances;
getNnarInstances(Instances, N) ->
	{ok, Pid} = nnar:start(self(),false),
	Instance = nnar:callbacks(Pid),
	getNnarInstances([Instance|Instances], N-1).

testWrite([]) -> ok;
testWrite([ Next | Rest]) ->
	{Inst,_} = Next,
	Val = length(Rest),
	nnar:write(Inst, Val),
	receive
		{nnar_donewrite, _} ->
			?assert(true)
		after 10000 ->
			?assert(false)
	end,
	checkRead([Next | Rest], Val).

checkRead([], _) -> ok;
checkRead([ {Inst,_} | Instances], MatchVal) ->
	nnar:read(Inst),
	receive
		{nnar_doneread, _, Val} ->
			?assertEqual(MatchVal, Val)
		after 5000 ->
			?assert(false)
	end,
	checkRead(Instances,MatchVal).

