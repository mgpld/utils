-module( task_scheduler_srv ).

-behaviour(gen_server).

-define( MAX_TRY,	3).
-define( START_DELAY,	5000). % time to wait after the file operation
-define( MAX_TIMEOUT,	5000). 
-define( TIMEOUT,	3000).

-include("../include/debug-console.hrl").

-export([
	start/0,
	start_link/0,
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
	verify/2,
	verify/1,
	add/1,
	add/2,
	add/3
]).

-record(state, {
		queries,	% usage stats
		errors}).
start() ->
	gen_server:start(?MODULE, [], []).

start_link() ->
	gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

-spec init(list()) ->
	{ok, tuple()}.
init([]) ->
	{ok, #state{
		queries = 0,
		errors = 0}}.

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

-spec verify( pid() | atom(), non_neg_integer() ) -> ok.
verify(Srv, Pid) ->
	gen_server:call(Srv, {verify, Pid}).

-spec verify( pid() | atom() ) -> ok.
verify(Pid) ->
	verify(?MODULE, Pid).

-spec flush( pid() | atom() ) -> ok.
flush(Pid) ->
	gen_server:cast(Pid, flush).

-spec flush() -> ok.
flush() ->
	flush(?MODULE).

-spec add({integer(), list(), list()}) -> ok | {error, atom()|list()}.
add({_, _, _} = Args) ->
	add( ?MODULE, Args).

-spec add( pid() | atom(), {integer(), list(), list()}) -> ok | {error, atom()|list()}.
add(Srv, {_, _, _} = Args) ->
	gen_server:call( Srv, {add, Args}).

-spec add( pid() | atom(), {integer(), list(), list()}, non_neg_integer()) -> 
	ok | {error, atom()|list()}.
add(Srv, {_, _, _} = Args, Timeout) ->
	gen_server:call( Srv, {add, Args}, Timeout).

handle_call(snap, _Node, #state{queries=Q} = State) ->
	{reply, {ok, State}, State#state{queries=Q+1}};

handle_call({add, {Id, Dir, Args}}, From, #state{queries=Q} = State) ->
	dump_args( Id, Dir, Args),
	Result = task_runner_srv:start_link(Dir, Id),
	{reply, Result, State#state{queries=Q+1}};

handle_call(_, _Node, State) ->
	{reply, undefined, State}.

handle_cast(stop, State) ->
	Reason = normal,
	{stop, Reason, State};

handle_cast(_Msg, State) ->
	{noreply, State}.

%
handle_info({'EXIT', _Pid, _Reason}, State) ->
	{noreply, State};

handle_info(_Info, State) ->
	?DEBUG("Unhandled Info: ~p\n", [ _Info ]),
	{noreply, State}.

%
terminate(_Reason, #state{queries=_Q} = _State) ->
	?DEBUG("Terminate, because '~p' served ~w requests~n", [ _Reason, _Q ]),
	ok.

code_change(_, State, _Vsn) ->
	{ok, State}.

dump_args(_, _, []) ->
	ok;
dump_args(TaskId, TaskType, Args) ->
	task_formatter:output(
		[ TaskType, "/args/", integer_to_list(TaskId) ], 
		Args).
