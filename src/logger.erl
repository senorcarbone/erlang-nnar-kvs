-module(logger).
-behavior(gen_server).

-export([start/1, stop/1, log/2]).
-export([init/1, handle_cast/2]).

-record(state, {logfile}).

start(Logname) ->
	gen_server:start_link(?MODULE, [Logname], []).

stop(Me) ->
	gen_server:cast(Me, stop).

log(Me, Msg) ->
	gen_server:cast(Me, {log, Msg}).

%%GEN_SERVER

init([Logname]) ->
	{ok, LogFile} = file:open(Logname, [append]),
	{ok, #state{logfile=LogFile}}.

handle_cast({log, Msg}, C=#state{}) ->
	file:write(C#state.logfile, erlang:list_to_binary(Msg)),
	{noreply, C};
handle_cast(stop, C=#state{}) ->
	file:close(C#state.logfile),
	{stop, normal, C}.
