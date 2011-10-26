-module(task_scheduler).
-export([display/1, display/2, schedule/1, test/0]).
-export([create_runnable/3, runnable/1, check_task/3]).
-export([gen_get_value/1, start_days/1, end_days/1]).
-export([launch/1]).
-export([get_schedule/1, get_script/1, get_start/1, get_stop/1, get_recurse/1]).
-export([schedule/2]).
-export([new_task/3, new_schedule/4, new_script/3, new_stop/2, new_recurse/2, new_validity/2]).
-export([minute/3, monday/1]).
-export([dump/1]).

-record(task, { name, script, schedule }).
-record(schedule, { validity, start, stop, recurse }).
-record(script, {name, args, conditions}).
-record(conditions, {need, provide}).
-record(recurse, {conditions, args}).


display(script, Arg) ->
	{ScriptName, Args, PreConditions, Results} = Arg,
	io:format("When ~p the execution of ~p(~p) posts ~p~n", [PreConditions, ScriptName, Args, Results]);

display(schedule, Arg) ->
	{Conditions, Recurse} = Arg,
	display(conditions, Conditions),
	display(recurse, Recurse).

display(Task) ->
	{ Scheduler, Recurse} = Task#task.schedule,
	{Task#task.name, Scheduler, Recurse}.
	

schedule([]) ->
	ok;	
schedule([ {Start, Conditions, Recurse} | Rest]) ->
	io:format("Task starts at ~p~n", [Start]),
	conditions(Conditions),
	recurse(Recurse),
	schedule(Rest).

conditions([]) ->
	ok;
conditions(#conditions{need=N, provide=P}) ->
	io:format("Conditions necessaires: ~p, conditions resultantes ~p~n", [N,P]),
	ok;
conditions([{first, Day} | Rest]) ->
	io:format("le premier ~p du mois, ", [humanday(Day)]),
	conditions(Rest);
conditions([{second, Day} | Rest]) ->
	io:format("le second ~p du mois, ", [humanday(Day)]),
	conditions(Rest);
conditions([{third, Day} | Rest]) ->
	io:format("le troisieme ~p du mois, ", [humanday(Day)]),
	conditions(Rest);
conditions([{fourth, Day} | Rest]) ->
	io:format("le quatrieme ~p du mois, ", [humanday(Day)]),
	conditions(Rest);
conditions([{last, Day} | Rest]) ->
	io:format("le dernier ~p du mois, ", [humanday(Day)]),
	conditions(Rest);
conditions([{day, MonthDay} | Rest]) ->
	io:format("le ~p du mois, ", [humanday(MonthDay)]),
	conditions(Rest);
conditions([{next, WeekDay} | Rest]) ->
	io:format("le prochain ~p, ", [humanday(WeekDay)]),
	conditions(Rest);
conditions([{exclude, List} | Rest]) ->
	io:format("a l'exclusion de ~p, ", [List]),
	conditions(Rest);
conditions([{weeks, X} | Rest]) ->
	io:format("toutes les ~p semaines ", [X]),
	conditions(Rest);
conditions([{months, X} | Rest]) ->
	io:format("tous les ~p mois ", [X]),
	conditions(Rest);
conditions([ {pre, List } | Rest]) ->
	io:format(" preconditions: ~p, ", [List]),
	conditions(Rest);
conditions([ {post, List } | Rest]) ->
	io:format(" postonditions: ~p, ", [List]),
	conditions(Rest);
conditions([_Any | Rest]) ->
	conditions(Rest).

recurse([]) ->
	ok;
recurse([ {seconds, Sec} | Rest ]) ->
	io:format("Seconds: ~p~n", [Sec]),
	recurse(Rest);
recurse([_Any | Rest]) ->
	recurse(Rest).

test() ->
	[ 
	new_task("backup1",
		new_script("backup.sh", "db1", new_conditions([db_down], [backup_ok])),
		new_schedule(   new_validity(notafter, "1y"), 
				{erlang:date(), erlang:time()}, 
				new_stop(relative, 50000), 
				new_recurse([], [ {weeks, 2} ]) )),
	new_task("backup2",
		new_script("fullbackup.sh", "ventes", new_conditions([lvm_down, db_down], [backup_ok])),
		new_schedule(   new_validity(notafter, "1y"), 
				{erlang:date(), erlang:time()}, 
				new_stop(relative, 50000), 
				new_recurse([], [ {months, 1} ]) ))
	].
				

schedule([], Acc) ->
	Acc;
schedule( [ {start, {Date, Time}} |  Rest ], Acc) ->
	schedule(Rest, [ {start, {Date, Time}} | Acc]);
schedule( [ {stop, {Date, Time}} | Rest ], Acc) ->
	schedule(Rest, [ {stop, {Date, Time}} | Acc]);
schedule( [ {recurse, { Conditions, RecurseArgs }} | Rest ], Acc) ->
	io:format("Conditions: ~p~nRecurse: ~p~n", [Conditions, RecurseArgs]),
	schedule(Rest, Acc);
schedule( [ _Any | Rest ], Acc) ->
	io:format("Unknown: ~p~n", [_Any]),
	schedule(Rest, Acc).

%backup de db tous les weekends

% #task{ 	name = backup_db,
% 	script = {"backup.sh", "ventes", [lvm_snapshot, db_locked], [backup_ok]},
% 	schedule = { [ { saturday, 1} ] } }.


runnable(Tasks) ->
	Date = erlang:date(),
	Time = erlang:time(),
	Dow = calendar:day_of_the_week(Date),
	Now = {Date, Time},
	create_runnable(Tasks, Now, Dow).

create_runnable([ Task | Rest], Now, Dow) ->
	case check_task(Task, Now, Dow) of
		start ->
			launch(Task),
			create_runnable(Rest, Now, Dow);
		
		_ ->
			create_runnable(Rest, Now, Dow)
	end.

check_task(#task{ name=_Name, script=_Script, schedule=Schedule }, _Now, Dow) ->
	{S, _} = Schedule, 
	StartDays = start_days(S),
	%EndDays = end_days(Schedule),
	case lists:member(Dow, StartDays) of
		true ->
			start;
		false ->
			no
	end.	
		
	
gen_get_value(Value) ->
	fun(List) ->
		case proplists:get_value(Value, List) of
			undefined ->
				[];
			V ->
				V
		end
	end.	
	
start_days(Schedule) ->
	Fun = gen_get_value(start),
	Fun(Schedule).

end_days(Schedule) ->
	Fun = gen_get_value(stop),
	Fun(Schedule).
	
launch(#task{name=Name, script=Script}) ->
	{ Command, Args, _Pre, _Post } = Script,
	io:format("Starting: ~p ( ~p ~p )~n", [Name, Command, Args]).
	
get_recurse(#task{schedule=S}) ->
	S#schedule.recurse.

get_schedule(#task{schedule=S}) ->
	S.

get_script(#task{script=S}) ->
	S.

get_start(#task{schedule=S}) ->
	S#schedule.start.

get_stop(#task{schedule=S}) ->
	S#schedule.stop.

new_script(Name, Args, Conditions) ->
	#script{name=Name, args=Args, conditions=Conditions}.

new_schedule(Validity, Start, Stop, Recurse) ->
	#schedule{validity=Validity, start=Start, stop=Stop, recurse=Recurse}.

new_stop(absolute, {Date, Time}) ->
	{absolute, {Date, Time}};
new_stop(relative, Time) ->
	{relative, Time};
new_stop(Date, Time) ->
	{absolute, {Date, Time}}.

new_recurse(Conditions, Args) ->
	#recurse{conditions=Conditions, args=Args}.

new_validity(notbefore, Before) ->
	[ {notbefore, Before} ];

new_validity(notafter, After) ->
	[ {notafter, After} ];

new_validity(Before, After) ->
	[ {notbefore, Before}, {notafter, After} ].
	
new_task(Name, Script, Schedule) ->
	#task{name=Name, script=Script, schedule=Schedule}.

new_conditions(Need, Provide) ->
	#conditions{need=Need, provide=Provide}.

minute(Module, Fun, Args) ->
	Val = new_validity(erlang:date(), {2008, 01, 01}),
	Script = new_script(Module, [Fun | Args], conditions),
	Recurse = new_recurse([], [ { minutes, 1} ]),
	Schedule = new_schedule(Val, erlang:date(), new_stop(relative, 3600), Recurse),
	new_task(minute, Script, Schedule).

monday(Message) ->
	Val = new_validity(erlang:date(), {2008, 01, 01}),
	Script = new_script(io, Message, [ {pre, [backup_ok]} ] ),
	Recurse = new_recurse([{ first, 1}, {second, 2}, {exclude, [ {2007,12,20} ]} ], [ { minutes, 1} ]),
	Schedule = new_schedule(Val, erlang:date(), new_stop(relative, 3600), Recurse),
	new_task(minute, Script, Schedule).
	
	
dump(#schedule{validity=Val, start=Start, stop=Stop, recurse=R}) ->
	io:format("Val: ~p~n", [Val]),
	io:format("Start: ~p~n", [Start]),
	io:format("Stop: ~p~n", [Stop]),
	dump(R);

dump(#recurse{conditions=C, args=_A}) ->
	conditions(C),
	conditions(_A);

dump(#task{name=Name, schedule=Schedule, script=Script}) ->
	dump(Schedule),
	dump(Script),
	Name;

dump(#script{name=N, args=A, conditions=C}) ->
	io:format("Script: commande ~p, arguments ~p~n", [N, A]),
	conditions(C).
	
	
humanday(1) -> "lundi";
humanday(2) -> "mardi";
humanday(3) -> "mercredi";
humanday(4) -> "jeudi";
humanday(5) -> "vendredi";
humanday(6) -> "samedi";
humanday(7) -> "dimanche".
