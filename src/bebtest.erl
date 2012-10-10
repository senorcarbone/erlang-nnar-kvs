-module(bebtest).
-export([testBeB/1]).
-include_lib("eunit/include/eunit.hrl").

testBeB(NodeNum) ->
	Processes = getBebInstances(NodeNum),
	?assertEqual(NodeNum, length(Processes)),
	configureBeB(Processes),
	testBroadcasts(Processes),
	ok.

getBebInstances(Num) ->
	addBebInstance([],Num).

configureBeB(Processes) ->
	lists:foreach(fun(Proc) -> beb:configure(Proc, Processes) end, Processes).

addBebInstance(Instances,0) ->
	Instances;
addBebInstance(Instances, N) ->
	{ok, Pid} = beb:start(self()),
	addBebInstance([Pid|Instances], N-1).

testBroadcasts(Processes) ->
	checkBroadcasts(Processes, Processes).

checkBroadcasts([], _Processes) -> ok;
checkBroadcasts([Proc|Pending], Processes) ->
	ReqCount = length(Processes),
	beb:broadcast(Proc, testMsg),
	testDeliver(ReqCount, testMsg),
	checkBroadcasts(Pending, Processes).

testDeliver(0, _Msg) -> ok;
testDeliver(Count, Msg) ->
	receive
		{bebdeliver, RecMsg} -> 
			?assertEqual(Msg,RecMsg),
			testDeliver(Count-1, Msg)
		after 2000 ->
			?assert(false)
	end.
