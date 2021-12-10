-module(day1).

-compile(export_all).

main(File) ->
    {ok, RawData} = file:read_file(File),
    Data = [ binary_to_integer(N) || N <- binary:split(RawData, <<"\n">>, [global, trim]) ],
    io:format("part 1: ~p~n", [solve1(Data)]),
    io:format("part 2: ~p~n", [solve2(Data)]).

solve1(Data) ->
    [Next|Rest] = lists:reverse(Data),
    solve1(Next, Rest).

solve1(Next, [Prev|Rest]) when Next > Prev ->
    1 + solve1(Prev, Rest);
solve1(_, [Prev|Rest]) ->
    solve1(Prev, Rest);
solve1(_, []) ->
    0.

solve2([One, Two, Three|Rest]) ->
    solve2(One + Two + Three, [Two, Three|Rest]).

solve2(SumA, [One, Two, Three|Rest]) ->
    SumB = One + Two + Three,
    N = case SumB > SumA of
            true  -> 1;
            false -> 0
        end,
    N + solve2(SumB, [Two, Three|Rest]);
solve2(_, _) ->
    0.