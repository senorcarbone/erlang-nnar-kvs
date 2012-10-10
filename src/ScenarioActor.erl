-module(scenarioactor).
-behavior(gen_server).

-export([start/0, startup/5, configure/2, callbacks/1, stop/1, run/1]).
-export([init/1, handle_call/3, fetchCallbacks/1, fetchCallbacks/2, setupExperiments/4, setExperiment/5]).

-record(state, {procs=[]}).

start() ->
	gen_server:start_link(?MODULE, [], []).

%%spawns the processes. Should be called by a remote coordinator
startup(Me, HowMany, Exp, ExpParams, Logger) ->
	gen_server:call(Me, {startup, HowMany, Exp, ExpParams, Logger}).

callbacks(Me) ->
	gen_server:call(Me, callbacks).

configure(Me, Configuration) ->
	gen_server:call(Me, {configure, Configuration}).

stop(Me) ->
	gen_server:cast(Me, stop).

run(Me) ->
	gen_server:call(Me, run).

%%GEN_SERVER

init([]) ->
	register(scenario, self()),
	{ok, #state{}}.

handle_call({startup, Ps, Exp, ExpParams, Logger}, _From, C=#state{}) ->
	Experiments = setupExperiments(Exp, ExpParams, Ps, Logger),
	{reply, ok, C#state{procs=Experiments}};
handle_call(callbacks,_From, C=#state{}) ->
	Callbacks = fetchCallbacks(C#state.procs),
	{reply, Callbacks, C};
handle_call(run, _From, C=#state{}) ->
	lists:foreach(fun(P)->experiment:run(P) end, C#state.procs),
	{reply, ok, C};
handle_call({configure, Configuration}, _From, C=#state{})->
	lists:foreach(fun(P)-> experiment:configure(P,Configuration) end, C#state.procs),
	{reply, ok, C}.

fetchCallbacks(Procs) ->
	fetchCallbacks([], Procs).

fetchCallbacks(Cb, []) -> Cb;
fetchCallbacks(L, [Next|Rest]) ->
	fetchCallbacks([experiment:callbacks(Next)|L], Rest).

setupExperiments(Exp, ExpParams, Ps, Logger) ->
	setExperiment(Exp, ExpParams,[], Ps, Logger).

setExperiment(_, _, L, 0, _) -> L;
setExperiment(Exp, ExpParams, L, Ps, Logger) ->
	{ok, Experiment} = experiment:start(Exp, ExpParams, Logger),
	setExperiment(Exp, ExpParams, [Experiment|L], Ps-1, Logger).

