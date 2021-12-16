-module(day15).

-compile(export_all).

main(File) ->
    {ok, RawData} = file:read_file(File),
    Data = [ [ X - $0 || X <- binary_to_list(Line) ]
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
    search(Map).

solve2(Map) ->
    search(expand(Map)).

search(Map) ->
    End   = lists:max(maps:keys(Map)),
    Start = {1, 1},
    search([{0, Start}], End, #{}, inf, Map).

search([], _, _Seen, Best, _) ->
    Best;
search([{Len, End}|Rest], End, Seen, Best, Map) when Len < Best ->
    NewBest = case Len < Best of
                 true  -> Len;
                 false -> Best
             end,
    search(Rest, End, Seen, NewBest, Map);
search([{Len, Coord}|Rest], End, Seen, Best, Map) ->
    case Len >= Best of
        true  -> search(Rest, End, Seen, Best, Map);
        false ->
            NewSeen  = Seen#{Coord => ok},
            ToFollow = lists:sort([ {Len + maps:get(C, Map), C} || C <- adjacent_coords(Coord, End, NewSeen) ]),
            search(ordsets:union(ToFollow, Rest), End, NewSeen, Best, Map)
    end.

adjacent_coords({X, Y}, {EndX, EndY}, Seen) ->
    [ {A, B} || {A, B} <- [{X + 1, Y},
                           {X, Y + 1},
                           {X - 1, Y},
                           {X, Y - 1}],
                          A =< EndX, B =< EndY,
                          A >= 1,    B >= 1,
                          not maps:is_key({A, B}, Seen) ].

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
