-module(task_test).

-export([launch/2, launch/3]).
-export([args/0]).
-export([test/1]).

% -spec launch( TaskId, TaskType, Args ) -> Result when
%	TaskId :: non_neg_integer(),
%	TaskType :: list(),
%	Args :: list(),
%	Result :: ok | {error, reason()}.

-spec launch( non_neg_integer(), list()) -> 
	ok | {error, atom()}.
launch( TaskId, TaskType ) ->
	launch( TaskId, TaskType, []).

-spec launch( non_neg_integer(), list(), list()) -> 
	ok | {error, atom()}.
launch( TaskId, TaskType, Args) ->
	task_scheduler_srv:add({ TaskId, TaskType, Args}).

% -spec dump_args( TaskId, TaskType, Args) -> Result when
%	TaskId :: non_net_integer(),
%	TaskType :: list(),
%	Args :: list(),
%	Result :: ok | {error, reason()}.

dump_args(_, _, []) ->
	ok;
dump_args(TaskId, TaskType, Args) ->
	task_formatter:output(
		[ TaskType, "/args/", integer_to_list(TaskId) ], 
		Args).

% task_manager:launch(200, "lbc", [{"dptcode","06"},{"zipcode", "06600"}]).
% Args = 
args() ->
	[
	{"region","21"},
	{"dptcode","6"},
	{"zipcode","06600"},
	{"category","27"},
	{"name","Publication"},
	{"email","mgpld.project@gmx.fr"},
	{"subject","Publicitor"},
	{"phone","0142321515"},
	{"price","47"},
	{"body","body.txt"},
	{"image","empty"} 
	].


test("free") ->
	Test = [
		{"email", "mgpld.project@gmx.fr"},
		{"file", "test"},
		{"pass", "none"}
	],
	launch(30, "dl.free.fr", Test);
test("lbc") ->
	launch(32, "lbc", args());
test(_) ->
	undefined.
