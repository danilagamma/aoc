-module(day15).

-compile(export_all).

main(File) ->
    {ok, RawData} = file:read_file(File),
    Data = [ [ X - $0 || X <- binary_to_list(Line) ]
             || Line <- binary:split(RawData, <<"\n">>, [global, trim]) ],
    Map = load(Data),
    io:format("part 1: ~p~n", [solve1(Map)]),
    io:format("part 2: ~p~n", [solve2(Map)]).

lp() ->
    F = "input/input15.txt",
    {ok, RawData} = file:read_file(F),
    Data = [ [ X - $0 || X <- binary_to_list(Line) ]
             || Line <- binary:split(RawData, <<"\n">>, [global, trim]) ],
    load(Data).

load(Data) ->
    maps:from_list([ {{X, Y}, N} || {Y, Line} <- enum(Data),
                      {X, N}                  <- enum(Line) ]).

enum(List) ->
    lists:zip(lists:seq(1, length(List)), List).

solve1(Map) ->
    find_min(Map).

solve2(Map) ->
    find_min(expand(Map)).

find_min(Map) ->
    End      = lists:max(maps:keys(Map)),
    Start    = {1, 1},
    ToFollow = gb_sets:singleton({Start, 0}),
    find_min(ToFollow, End, #{Start => ok}, inf, Map).

find_min([],  _End, _Visited, Min, _Map) -> Min;
find_min(Set,  End,  Visited, Min,  Map) ->
    case gb_sets:is_empty(Set) of
        true ->
            Min;
        false ->
            {{Coord, Len}, NewSet} = gb_sets:take_smallest(Set),
            case Len > Min of
                true  -> find_min(NewSet, End, Visited, Min, Map);
                false ->
                    case Coord of
                        End -> find_min(NewSet, End, Visited, Len, Map);
                        _   ->
                            NewSet2 = lists:foldl(fun (C, Acc) -> gb_sets:add_element({C, Len + maps:get(C, Map)}, Acc) end,
                                                  NewSet,
                                                  adjacent_coords(Coord, End, Visited)),
                            find_min(NewSet2, End, Visited#{Coord => ok}, Min, Map)
                    end
            end
    end.

dijkstra(Map) ->
    End      = lists:max(maps:keys(Map)),
    Start    = {1, 1},
    dijkstra([{0, Start}], End, #{}, #{}, Map).

dijkstra([], _, _, Acc, _) ->
    Acc;
dijkstra([{Len, Coord}|Rest], End, Seen, Acc, Map) ->
    %io:format("~p~n", [Acc]),
    Adjacent = [ {Len + maps:get(C, Map), C} || C <- adjacent_coords(Coord, End, Seen) ],
    NewAcc = lists:foldl(fun ({C, L}, A) ->
                                 case L < maps:get(C, Acc, inf) of
                                     true  -> A#{C => L};
                                     false -> A
                                 end
                         end,
                         Acc,
                         Adjacent),
    dijkstra(lists:sort(Adjacent ++ Rest), End, Seen#{Coord => ok}, NewAcc#{Coord => Len}, Map).


adjacent_coords({X, Y}, {EndX, EndY}, Visited) ->
    [ {A, B} || {A, B} <- [{X + 1, Y},
                           {X - 1, Y},
                           {X, Y - 1},
                           {X, Y + 1}],
                           A =< EndX, B =< EndY,
                           A >= 1,    B >= 1,
                           not maps:is_key({A, B}, Visited) ].

wrap(V) ->
    max(1, V rem 10).

expand(Map) ->
    {_, Shift} = lists:max(maps:keys(Map)),
    lists:foldl(fun maps:merge/2, #{},
                maps:values(lists:foldl(fun (C, A) -> do_expand(C, Shift, A) end,
                                        #{{1, 1} => Map},
                                        [ {X, Y} || X <- lists:seq(1, 5),
                                                    Y <- lists:seq(1, 5) ]))).

do_expand({1,  1 }, _Shift, Acc) -> Acc;
do_expand({Xm, Ym},  Shift, Acc)
  when Xm =:= 1 ->
    Upper = maps:get({Xm, Ym - 1}, Acc),
    Acc#{
        {Xm, Ym} => maps:from_list([ {{X, Shift + Y}, wrap(V + 1)} || {{X, Y}, V} <- maps:to_list(Upper) ])
    };
do_expand({Xm, Ym}, Shift, Acc) ->
    Upper = maps:get({Xm - 1, Ym}, Acc),
    Acc#{
        {Xm, Ym} => maps:from_list([ {{Shift + X, Y}, wrap(V + 1)} || {{X, Y}, V} <- maps:to_list(Upper) ])
    }.
