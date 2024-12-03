-module(day02).

-export([part_one/1, part_two/1]).

part_one(Input) ->
	Lists = util:read_lines("d2test.txt"),
	SafeLists = lists:map(fun(List) -> util:safe_list(util:convert(List)) end, Li),
	lists:sum([if L -> 1; true -> 0 end || L <- SafeLists]).

part_two() ->
	
