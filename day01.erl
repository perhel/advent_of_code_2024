-module(day01).

-export([read_as_lists/1, part_one/1, part_two/1]).

part_one(Input) ->
    {Left, Right} = read_as_lists(Input),
    SortedLeft = lists: sort(Left),
    SortedRight = lists:sort(Right),
    Distances = lists:zipwith(fun(L, R) -> abs(binary_to_integer(L) - binary_to_integer(R)) end,
    SortedLeft, SortedRight),
    lists:sum(Distances).

part_two(Input) ->
    {Left, Right} = read_as_lists(Input),
    lists:foldl(fun(Num, Acc) ->
        Acc + binary_to_integer(Num) * length(
            lists:filter(fun(N) -> N == Num end, Right)
        ) end,
    0, Left).

read_as_lists(Input) ->
    {ok, Binary} = file:read_file(Input),
    Lines = binary:split(Binary, <<"\n">>, [global]),
    lists:foldl(fun(Line, {AccLeft, AccRight}) ->
            case binary:split(Line, <<"   ">>, [global]) of [L, R] ->
                {[L | AccLeft], [R | AccRight]};
            _ -> {AccLeft, AccRight}
        end
    end, {[], []}, Lines).