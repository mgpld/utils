-module( task_runner_srv ).

-behaviour(gen_server).

-define( MAX_TRY,	3).
-define( START_DELAY,	5000). % time to wait after the file operation
-define( MAX_TIMEOUT,	5000). 
-define( TIMEOUT,	3000).

-include("../include/debug-console.hrl").

-export([start/0, 
	start_link/0,
	start_link/1,
	start_link/2,
	stop/1, stop/0]).

-export([
	init/1,
	handle_call/3,
	handle_cast/2,
	handle_info/2,
	terminate/2,
	code_change/3
]).

-export([
	snap/1, snap/0,
	flush/1, flush/0,
	add/2,
	add/1
]).

-record(state, {
		dir,		% working directory
		fd,		% log file
		count, 		% current iteration count
		max,		% max iterations allowed
		timeout, 	% timeout between iterations
		script,		% script name
		arguments,	% script arguments
		port,		% program <port>
		queries,	% usage stats
		errors}).
start() ->
	gen_server:start(?MODULE, [], []).

start_link() ->
	gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

start_link( Dir ) ->
	gen_server:start_link( ?MODULE, [ Dir ], []).

start_link( Dir, Id ) ->
	gen_server:start_link( ?MODULE, [ Dir, Id ], []).

init([ Dir ]) ->
	init([ Dir, ?MAX_TIMEOUT ]);

init([ Dir, Id ]) ->
	Root = case application:get_env( dir ) of
		{ok, R} ->
			R;
		undefined ->
			"."
	end,
	{ok, #state{
			dir = Root ++ "/" ++ Dir,
			fd = undefined,
			max = ?MAX_TRY,
			timeout = ?TIMEOUT,
			script = "run",
			arguments = Id, %"Workspace/args",
			count = ?MAX_TRY,
			port = undefined,
			queries = 0,
			errors = 0}, ?TIMEOUT}.

-spec stop( pid() | atom() ) ->  ok.
stop(Srv) ->
	gen_server:cast(Srv, stop).

-spec stop() -> ok.
stop() ->
	stop(?MODULE).

-spec snap( pid() | atom() ) -> ok.
snap(Srv) ->
	gen_server:call(Srv, snap).

-spec snap() -> ok.
snap() ->
	snap(?MODULE).

-spec flush( pid() | atom() ) -> ok.
flush(Pid) ->
	gen_server:cast(Pid, flush).

-spec flush() -> ok.
flush() ->
	flush(?MODULE).

-spec add({ integer(), term() }) -> ok.
add({_Key, _Struct} = Value) ->
	add(?MODULE, Value).

-spec add( pid() | atom(), {integer(), term()}) -> ok.
add(Srv, {_Key, _Struct} = Value) ->
	gen_server:cast(Srv, {add, Value}).

handle_call(snap, _Node, #state{queries=Q} = State) ->
	{reply, {ok, State}, State#state{queries=Q+1}};

handle_call(_, _Node, State) ->
	{reply, undefined, State}.

handle_cast(run, #state{ count=Count, timeout = Timeout } = State) ->
	NewState = run( State ),
	{noreply, NewState#state{count=Count - 1}, Timeout};

handle_cast(stop, State) ->
	Reason = normal,
	{stop, Reason, State};

handle_cast(_Msg, State) ->
	{noreply, State}.

%
handle_info({'EXIT', _Pid, _Reason}, State) ->
	{noreply, State};

handle_info({Port, {exit_status, 0}}, #state{port=Port, script= Script} = State) ->
	?DEBUG("Script: ~p, successfully ended with: ~p\n", [ Script, 0 ]),
	{stop, normal, State};

handle_info({Port, {exit_status, _Status}}, #state{
	port=Port, script= Script, timeout = Timeout,
	fd=Fd} = State) ->

	?DEBUG("Script: ~p, ended with error: ~p\n", [ Script, _Status ]),

	case Fd of
		undefined ->
			{noreply, State#state{port=undefined}, Timeout};
		_ ->
			ok = file:close(Fd),
			{noreply, State#state{port=undefined, fd=undefined}, Timeout}
	end;

handle_info({Port, {data, Bin}}, #state{port=Port, script= _Script} = State) ->
	%?DEBUG("DISPLAY: ~p: ~p\n", [ Script, Bin ]),
	NewState = parse_output(Bin, State), 
	{noreply, NewState};

handle_info(timeout, #state{count = 0, max = MaxTry} = State) ->
	?DEBUG("Tried ~w times, dying\n", [MaxTry]),
	%gen_server:cast(?MODULE, run),
	%{noreply, State#state{count = MaxTry}};
	{stop, normal, State};

handle_info(timeout, #state{count = Count} =  State) ->
	?DEBUG("Start Task (next iterations: ~w)\n", [ Count ]),
	NewState = run( State ),
	{noreply, NewState#state{count = Count - 1}}.

%
terminate(_Reason, #state{queries=_Q} = _State) ->
	?DEBUG("Terminate, because '~p' served ~w requests~n", [ _Reason, _Q ]),
	ok.

code_change(_, State, _Vsn) ->
	{ok, State}.

% internals
-spec run( tuple() ) -> tuple().
run( #state{ 
	count = _Count,
	dir = Dir, 
	script = Script, 
	arguments = Arguments } = State ) ->	

	% JobId = day_of_year() + Count,
	Executable = Dir ++ "/" ++ Script,
	?DEBUG("Starting script: ~p, with args: ~p\n", [ Executable, Arguments ]),
	 Port = open_port({ spawn, Dir ++ "/" ++ Script ++
				" " ++ integer_to_list(Arguments) }, [
	 				exit_status, binary
	 			]),

				% " " ++ integer_to_list(JobId) ++
				% " " ++ Arguments }, [

	State#state{ port = Port }.
%
% 2011/08/03T18:29:34
parse_output(Bin, State) ->
	do_parse(Bin, State).
	
% TimeStamp 2011/08/19T07:37:12
%do_parse(<<TimeStamp:19/binary, " ", Rest/binary>>, State) ->
%	?DEBUG("Info ~p: ~p\n", [ TimeStamp, Rest ]),
%	State;

do_parse(Bin, #state{dir=Dir,fd=undefined,count=Iter,arguments=Arg} = State) ->
	% Year = element(1, erlang:date()),
	% {DayOfYear, _} = calendar:time_difference({
	% 	{Year,1,1},{0,0,0}}, 
	% 	calendar:local_time()),
	FileName = [ Dir, "/logs/", integer_to_list(Arg), ".",
		%integer_to_list(DayOfYear), ".",
		integer_to_list(Iter), ".log" ],
	case file:open(FileName, [raw,write,binary]) of
		{ok, Fd} ->
			ok = file:write(Fd, Bin),
			State#state{fd=Fd};

		{error, _Error} ->
			State
	end;

do_parse(Bin, #state{fd=Fd} = State) ->
	ok = file:write(Fd, Bin),
	State;

do_parse(Bin, State) ->
	?DEBUG("unhandled: ~p\n", [ Bin ]),
	State.

% day_of_year() ->
% 	{Day, _} = calendar:time_difference({
% 		{element(1, erlang:date()),1,1},{0,0,0}}, 
% 		calendar:local_time()),
% 	Day.
