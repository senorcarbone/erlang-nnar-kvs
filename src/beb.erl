-module(beb).
-beheavior(gen_server).

-export([start/1, stop/1, broadcast/2, configure/2]).
-export([init/1, handle_call/3, handle_cast/2, terminate/2]).

-record(config, {nodes, callee}).

%%PUBLIC API

start(Callee) ->
	gen_server:start_link(?MODULE, [Callee], []).

broadcast(Pid, Msg) ->
	% io:format("Broadcasting msg ~p via pid ~p",[Msg,Pid]),
	gen_server:call(Pid, {bebbroadcast, Msg}).

configure(Pid, Nodes) ->
	gen_server:call(Pid, {reconfigure, Nodes}).

stop(Pid) ->
	gen_server:cast(Pid, stop).

%% GEN_SERVER 

init([Callee]) ->
	{ok, #config{callee=Callee}}.

terminate(_Reason, _State) ->
	ok.
	
handle_call({bebbroadcast, Msg}, _From, C=#config{}) ->
	lists:foreach(fun(Node) -> gen_server:cast(Node, {pp2psend,Msg}) end, C#config.nodes),
	{reply, ok, C};
handle_call({reconfigure, UpdNodes}, _From, C=#config{}) ->
	{reply, ok, C#config{nodes=UpdNodes}}.

handle_cast({pp2psend,Msg}, C=#config{}) ->
	C#config.callee ! {bebdeliver, Msg},
	% io:format("pp2psend delivered : callee: ~p ~n",[C#config.callee]),
	{noreply, C};
handle_cast(stop, C=#config{}) ->
	{stop, normal, C}.
