-module(day02).

-export([
	part_one/1,
	part_two/1,
	except/2,
	scan_with_exclude/1
	]).

part_one(Input) ->
	Lists = util:read_lines(Input),
	SafeLists = lists:map(fun(List) -> util:safe_list(util:convert(List)) end, Lists),
	lists:sum([if L -> 1; true -> 0 end || L <- SafeLists]).

part_two(Input) ->
	Lists = util:read_lines(Input),
	FixableScores = lists:map(fun(List) -> day02:scan_with_exclude(util:convert(List)) end, Lists),
	length(lists:filter(fun(List) -> lists:min(List) < 1 end, FixableScores)).

scan_with_exclude(List) ->
	[util:safe_score(day02:except(I, List)) || {I, _} <- lists:enumerate(List)].

except(Index, List) ->
	Indexed = lists:enumerate(List),
	FilteredIndexed = lists:filter(fun(Item) -> {I, _} = Item, I /= Index end, Indexed),
	lists:map(fun(Item) -> {_, V} = Item, V end, FilteredIndexed).