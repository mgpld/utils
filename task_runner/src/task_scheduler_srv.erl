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
	add/1,
	add/2,
	add/3,
	all/0, all/1
]).

-record(state, {
		dir,
		queries,	% usage stats
		errors}).
start() ->
	gen_server:start(?MODULE, [], []).

start_link() ->
	gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

-spec init(list()) ->
	{ok, tuple()}.
init([]) ->
	Root = case application:get_env( dir ) of
		undefined ->
			".";
		{ok, Value} ->
			Value
	end,
	{ok, #state{
		dir=Root,
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

-spec all() ->
	{ok, list()} | {error, atom()}.
all() ->
	all( ?MODULE ).

-spec all( pid() | atom() ) ->
	{ok, list()} | {error, atom()}.
all( Srv ) ->
	gen_server:call( Srv, all ).

handle_call(snap, _Node, #state{queries=Q} = State) ->
	{reply, {ok, State}, State#state{queries=Q+1}};

handle_call({add, {Id, Dir, Args}}, _From, #state{queries=Q} = State) ->
	{Result, NewState} = do_add( Id, Dir, Args, State),
	{reply, Result, NewState#state{queries=Q+1}};

handle_call(all, _From, State) ->
	{Result, NewState} = do_list_jobs(State),
	{reply, Result, NewState};

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

-spec dump_args( non_neg_integer(), list(), list()) ->
	ok | {error, atom()}.	
dump_args(_, _, []) ->
	ok;
dump_args(TaskId, TaskType, Args) ->
	task_formatter:output(
		[ TaskType, "/args/", integer_to_list(TaskId) ], 
		Args).

% internals
do_list_jobs(#state{dir=Dir} = State) ->
	{ok, Result } = file:list_dir( Dir ),
	{Result, State}.
					
do_add( Id, Dir, Args, #state{dir=Root} = State ) ->
	FullPath = Root ++ "/" ++ Dir,
	ok = dump_args( Id, FullPath, Args),
	Result = task_runner_srv:start_link(Dir, Id), 
	{Result, State}.
