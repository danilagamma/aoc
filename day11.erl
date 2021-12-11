-module(day11).

-compile(export_all).

main(File) ->
    {ok, RawData} = file:read_file(File),
    Data = [ [ N - $0 || N <- binary_to_list(Line) ]
             || Line <- binary:split(RawData, <<"\n">>, [global, trim]) ],
    Map = load(Data),
    io:format("part 1: ~p~n", [solve1(Map)]),
    io:format("part 2: ~p~n", [solve2(Map)]).

load(Data) ->
    maps:from_list([ {{X, Y}, N} || {Y, Line} <- enum(Data),
                                    {X, N}    <- enum(Line) ]).

enum(List) ->
    lists:zip(lists:seq(1, length(List)), List).

solve1(Map) ->
    simulate(Map, 0, 100).

simulate(_Map, Count, 0)    -> Count;
simulate( Map, Count, Step) ->
    {NewMap, NewCount} = simulate_iter(maps:map(fun (_, V) -> V + 1 end, Map), Count),
    case (NewCount - Count) =:= maps:size(Map) of
        true  -> -Step;
        false -> simulate(NewMap, NewCount, Step - 1)
    end.

simulate_iter(Map, Count) ->
    {NewMap, NewCount} = maps:fold(fun simulate_iter/3, {Map, Count}, Map),
    case NewCount > Count of
        true  -> simulate_iter(NewMap, NewCount);
        false -> {NewMap, NewCount}
    end.

simulate_iter(XY, V, {Map, Count}) ->
    case V > 9 of
        true ->
            NewMap   = lists:foldl(fun (Adj, M) -> flash(Adj, M) end, Map, neighbours(XY)),
            NewCount = Count + 1,
            {NewMap#{XY => 0}, NewCount};
        false ->
            {Map, Count}
    end.

flash(Adj, M) ->
    NewV = case maps:get(Adj, M) of
               0 -> 0;
               V -> V + 1
           end,
    maps:put(Adj, NewV, M).

neighbours({X, Y}) ->
    [{X0, Y0} || X0 <- lists:seq(X - 1, X + 1),
                 Y0 <- lists:seq(Y - 1, Y + 1),
                 X0 >= 1, X0 =< 10, Y0 >= 1, Y0 =< 10 ]
 -- [{X, Y}].

solve2(Map) ->
    simulate(Map, 0, -1).