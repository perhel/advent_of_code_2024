-module(util).

-export([
	 read_lines/1,
	 convert/1,
	 ascending/1,
	 ascending_score/2,
	 descending/1,
	 descending_score/2,
	 valid_jump/1,
	 jump_score/2,
	 in_order/1,
	 order_score/1,
	 safe_list/1,
	 safe_score/1
]).

read_lines(Input) ->
    	{ok, Binary} = file:read_file(Input),
    	Lines = binary:split(Binary, <<"\r\n">>, [trim,global]),
	[binary:split(Line, <<" ">>, [trim,global]) || Line <- Lines].

ascending([]) -> true;
ascending([_]) -> true;
ascending([F, S | Rest]) when F < S ->
	ascending([S | Rest]);
ascending(_) -> false.

descending([]) -> true;
descending([_]) -> true;
descending([F, S | Rest]) when F > S ->
	descending([S | Rest]);
descending(_) -> false.

valid_jump([]) -> true;
valid_jump([_]) -> true;
valid_jump([F, S | Rest]) when abs(F - S) < 4 ->
	valid_jump([S | Rest]);
valid_jump(_) -> false.

in_order(List) ->
	ascending(List) orelse descending(List).

order_score(List) ->
	AScore = ascending_score(List, 0),
	DeScore = descending_score(List, 0),
	lists:min([AScore,DeScore]).

safe_list(List) ->
	in_order(List) andalso valid_jump(List).

safe_score(List) ->
	order_score(List) + jump_score(List, 0).

convert(BinaryList) ->
	[binary_to_integer(Bin) || Bin <- BinaryList].

ascending_score([], Acc) -> Acc;
ascending_score([_], Acc) -> Acc;
ascending_score([F, S | Rest], Acc) when F < S -> 
	ascending_score([S | Rest], Acc);
ascending_score([F, _ | Rest], Acc) ->
	ascending_score([F | Rest], Acc + 1);
ascending_score(_, Acc) -> Acc.

descending_score([], Acc) -> Acc;
descending_score([_], Acc) -> Acc;
descending_score([F, S | Rest], Acc) when F > S -> 
	descending_score([S | Rest], Acc);
descending_score([F, _ | Rest], Acc) ->
	descending_score([F | Rest], Acc + 1);
descending_score(_, Acc) -> Acc.

jump_score([], Acc) -> Acc;
jump_score([_], Acc) -> Acc;
jump_score([F, S | Rest], Acc) when abs(F - S) < 4 ->
	jump_score([S | Rest], Acc);
jump_score([F, _ | Rest], Acc) ->
	jump_score([F | Rest], Acc + 1);
jump_score(_, Acc) -> Acc.
