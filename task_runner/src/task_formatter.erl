-module(task_formatter).

-export([output/2]).

%-spec output( FileName, List ) -> Result when
%	Filename :: list() | tuple(),
%	List:: list(), 
%	Result :: any() ).

output(_, []) ->
	{error, eol};
output(FileName, List) when is_list(FileName) -> 
	case file:open(FileName, [raw,write,binary]) of
		{ok, Fd} ->
			do(Fd, List);
		{error, _} = Err ->
			Err
	end;
output( Fd, List ) ->
	do( Fd, List).

%-spec do ( Fd, List ) -> Result when
%	Fd :: tuple(),
%	List :: list(),
%	Result :: ok | {error, reason().


do( Fd, []) ->
	ok;
do( Fd, [ {K, V} | Rest ]) when is_binary(V) ->
	Length = size(V),
	Key = bin( K ),
	write( Fd, Key, Length, V ),
	do( Fd, Rest );

do( Fd, [ {K, V} | Rest ]) when is_list(V) ->
	Length = length(V),
	Key = bin( K ),
	write( Fd, Key, Length, V ),
	do( Fd, Rest ).

bin( Arg ) when is_atom(Arg) ->
	atom_to_binary(Arg, utf8);
bin( Arg ) ->
	Arg.

%bin( Arg ) when is_list(Arg) ->
%	Arg;
%bin( Arg ) when is_binary(Arg) ->
%	Arg;

write( Fd, Key, Length, Value) ->
	file:write(Fd,
		[ integer_to_list(Length), ":", Key, "\n", 
			Value, "\n" ]).

