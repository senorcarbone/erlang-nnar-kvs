-module(coordinator).
-behavior(gen_server).
-record(config, {state, nodes, scenarios, exp, logger}).

-export([start/1]).
-export([init/1, handle_info/2]).

%% {Exp, NodeList, PperNode, Params} ,eg. 
%%{kvs,[carbone@home, lars@home], 10, {1000, 5000, 30000}, 15000}
start(Configuration) ->
	gen_server:start_link(?MODULE, [Configuration], []).

%% gen_server
init([{Exp, NList, PperNode, Params, Timeout}]) ->
	register(coordinator, self()),
	{ok, Logger} = logger:start(io_lib:format("~p~B.txt", [Exp,PperNode])),
	io:format("~n connecting to remote scenarios...",[]),
	Scenarios = lists:foldl(fun(N,Acc)-> [{scenario, N}|Acc] end, [], NList),
	%%trigger scenarios
	io:format("~n preconfiguring scenarios...", []),
	lists:foreach(fun(Scen)-> scenarioactor:startup(Scen, PperNode, Exp, Params, Logger) end, Scenarios),
	io:format("~n gathering configuration information..."),
	AllConfig = lists:foldl(fun(C,Acc)-> scenarioactor:callbacks(C)++Acc end, [],Scenarios),
	TotalConfig =
	case Exp of
		nnar ->
			AllConfig;
		kvs -> 
			TotalNodes = length(AllConfig),
			ReadQuorum = (TotalNodes div 2),
			WriteQuorum =  ReadQuorum + 1,
			{AllConfig, {WriteQuorum, ReadQuorum}}
	end,
	io:format("~n applying batch configuration...", []),
	lists:foreach(fun(S)->scenarioactor:configure(S, TotalConfig) end, Scenarios),
	io:format("~n initializing the experiments...", []),
	lists:foreach(fun(S)->scenarioactor:run(S) end, Scenarios),
	erlang:send_after(Timeout, self(), stop),
	{ok, #config{state=running, nodes= NList, scenarios=Scenarios, exp=Exp,logger=Logger}}.


handle_info(stop, C=#config{}) ->
	logger:stop(C#config.logger),
	{stop, normal, C}.



