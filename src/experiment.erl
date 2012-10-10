-module(experiment).
-behavior(gen_server).

-export([start/3, configure/2, callbacks/1, run/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, startOp/1, timeOp/2, log/4, now_millis/0]).

-record(configuration, {app, time_startexp, scenario,appid,rs_updateinterval, time_preload=1000, time_warmup, time_measure, warmup=false, startTS, finishTS, logger}).

start(App,Params,Logger) ->
	gen_server:start_link(?MODULE, [App,self(),Params, Logger],[]).

configure(ExpPid, {View,Quorums}) ->
	gen_server:call(ExpPid, {configure, View, Quorums});
configure(ExpPid, View) ->
	gen_server:call(ExpPid, {configure, View}).

callbacks(ExpPid) ->
	gen_server:call(ExpPid, callbacks).

run(ExpPid) ->
	gen_server:cast(ExpPid, run).

%%gen_server

init([nnar,ScenarioActor, {WarmupT, MeasureT}, Logger]) ->
	{ok, Pid} = nnar:start(self(), true),
	{ok, #configuration{app=nnar, appid=Pid, scenario=ScenarioActor, time_warmup=WarmupT, time_measure=MeasureT,logger=Logger}};
init([kvs, ScenarioActor, {Rs_updinterval, WarmupT, MeasureT}, Logger]) ->
	{ok, Pid} = kvs:start(self(), 1000),
	{ok, #configuration{app=kvs, appid=Pid, scenario=ScenarioActor, rs_updateinterval=Rs_updinterval, time_warmup=WarmupT, time_measure=MeasureT,logger=Logger}}.

handle_call(callbacks, _From, C=#configuration{}) when C#configuration.app == nnar ->
	{reply, nnar:callbacks(C#configuration.appid), C};
handle_call(callbacks, _From, C=#configuration{}) when C#configuration.app == kvs ->
	{reply, kvs:callbacks(C#configuration.appid), C};

handle_call({configure, Configuration}, _From, C=#configuration{})  when C#configuration.app == nnar ->
	nnar:configure(C#configuration.appid, Configuration),
	{reply, ok, C};
handle_call({configure,View,Quorums}, _From, C=#configuration{})  when C#configuration.app == kvs ->
	kvs:configure(C#configuration.appid, View, Quorums),
	{reply, ok, C}.

handle_cast(run, C=#configuration{}) ->
	erlang:send_after(C#configuration.time_preload, self(), warmup),
	{noreply, C#configuration{time_startexp=now_millis()}}.
handle_info(warmup, C=#configuration{}) ->
	io:format("~n[Exp:~p]Starting warmup phase...",[self()]),
	erlang:send_after(C#configuration.time_warmup, self(), measure),
	UpC =startOp(C),
	{noreply, UpC#configuration{warmup=true}};
handle_info(measure, C=#configuration{})  ->
	io:format("~n[Exp:~p]Starting measuring phase...",[self()]),
	erlang:send_after(C#configuration.time_measure, self(), finish),
	UpC = startOp(C),
	{noreply, UpC#configuration{warmup=false}};
handle_info(finish, C=#configuration{}) when C#configuration.app == nnar ->
	io:format("~n[Exp:~p]Finished measuring. Stopping components ...",[self()]),
	nnar:stop(C#configuration.appid),
	{stop, normal, C};
handle_info(finish, C=#configuration{}) when C#configuration.app == kvs ->
	io:format("~n[Exp:~p]Finished measuring. Stopping components ...",[self()]),
	kvs:stop(C#configuration.appid),
	{stop, normal, C};
handle_info({nnar_doneread,_,_}, C=#configuration{}) ->
	timeOp("READ", C),
	{noreply, startOp(C)};
handle_info({nnar_donewrite,_}, C=#configuration{}) ->
	timeOp("WRITE", C),
	{noreply, startOp(C)};
handle_info({kvsputreply,_}, C=#configuration{}) ->
	timeOp("READ", C),
	{noreply, startOp(C)};
handle_info({kvsgetreply,_,_}, C=#configuration{}) ->
	timeOp("WRITE", C),
	{noreply, startOp(C)}.

startOp(C=#configuration{}) when C#configuration.app == nnar ->
	Stime = now_millis(),
	case random:uniform() > 0.5 of
		true -> nnar:read(C#configuration.appid);
		false ->
			Val = "abcdefghijklmnopq",
			nnar:write(C#configuration.appid, Val)
	end,
	C#configuration{startTS=Stime};
startOp(C=#configuration{}) when C#configuration.app == kvs ->
	Stime = now_millis(),
	NextKey = random:uniform(11),
	case random:uniform() > 0.5 of
		true -> kvs:get(C#configuration.appid, NextKey);
		false ->
			Val = "abcdefghijklmnopq",
			kvs:put(C#configuration.appid, NextKey, Val)
	end,
	C#configuration{startTS=Stime}.

timeOp(OpType, C=#configuration{}) ->
	CurTs = now_millis(),
	log(CurTs-C#configuration.time_startexp, OpType, CurTs-C#configuration.startTS,C#configuration.logger),
	ok.

log(Ts, Op, Diff, Logger) ->
	Msg = lists:flatten(io_lib:format("~n~B ~s ~B", [Ts,Op,Diff])),
	logger:log(Logger, Msg),
	ok.

now_millis() ->
    {Mega,Sec,Micro} = erlang:now(),
    (Mega*1000000+Sec)*1000000+Micro.





