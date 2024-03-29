%% Copyright (c) 2011, Mgpld <mgpld@free.fr>
%%
%% Permission to use, copy, modify, and/or distribute this software for any
%% purpose with or without fee is hereby granted, provided that the above
%% copyright notice and this permission notice appear in all copies.
%%
%% THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
%% WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
%% MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
%% ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
%% WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
%% ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
%% OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.

%% @private
-module(task_scheduler_app).
-behaviour(application).

-export([start/2, stop/1, profile_output/0]). %% API.

-type application_start_type() :: normal
	| {takeover, node()} | {failover, node()}.

%% API.

-spec start(application_start_type(), any()) -> {ok, pid()}.
start(_Type, _Args) ->
	%consider_profiling(),
	task_scheduler_sup:start_link().

-spec stop(any()) -> ok.
stop(_State) ->
	ok.

-spec profile_output() -> ok.
profile_output() ->
	eprof:stop_profiling(),
	eprof:log("procs.profile"),
	eprof:analyze(procs),
	eprof:log("total.profile"),
	eprof:analyze(total).

%% Internal.

-spec consider_profiling() -> profiling | not_profiling.
consider_profiling() ->
	case application:get_env(profile) of
		{ok, true} ->
			eprof:start(),
			eprof:start_profiling([self()]);
		_ ->
			false
	end.
