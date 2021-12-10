-module(day6).

-compile(export_all).

main(File) ->
    {ok, RawData} = file:read_file(File),
    Data = [ binary_to_integer(X) || X <- binary:split(RawData, <<",">>, [global, trim]) ],
    io:format("part 1: ~p~n", [solve1(Data)]),
    io:format("part 2: ~p~n", [solve2(Data)]),
    ok.

solve1(Data) ->
    length(simulate_n(Data, 80)).

simulate_n(Data, Days) ->
    lists:foldl(fun (_, Acc) -> [ NewFishes || F <- Acc, NewFishes <- simulate(F) ] end,
                Data,
                lists:seq(1, Days)).

simulate(0) -> [6, 8];
simulate(X) -> [X - 1].

solve2(Data) ->
    OneToFive = maps:values(freq(Data)),
    Initial = list_to_tuple([0] ++ OneToFive ++ [0, 0, 0]),
    lists:sum(tuple_to_list(simulate_count_n(Initial, 256))).

simulate_count_n(Acc, 0) -> Acc;
simulate_count_n(Acc, N) ->
    simulate_count_n(simulate_count(Acc), N - 1).

simulate_count({V0, V1, V2, V3, V4, V5, V6, V7, V8}) ->
    {V1, V2, V3, V4, V5, V6, V7 + V0, V8, V0}.

freq(List) ->
    lists:foldl(fun (K, Acc) -> maps:update_with(K, fun (V) -> V + 1 end, 1, Acc) end,
                #{},
                List).


