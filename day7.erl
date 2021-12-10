-module(day7).

-compile(export_all).

main(File) ->
    {ok, RawData} = file:read_file(File),
    Data = [ binary_to_integer(X) || X <- binary:split(RawData, <<",">>, [global, trim]) ],
    io:format("part 1: ~p~n", [solve1(Data)]),
    io:format("part 2: ~p~n", [solve2(Data)]),
    ok.

solve1(Data) ->
    run(fun(X) -> X end, Data).

run(FuelFun, Data) ->
    lists:min([ lists:sum([ FuelFun(abs(D - N)) || D <- Data ])
                || N <- lists:seq(lists:min(Data), lists:max(Data)) ]).

solve2(Data) ->
    run(fun fuel/1, Data).

fuel(X) ->
    X * (1 + X) div 2.