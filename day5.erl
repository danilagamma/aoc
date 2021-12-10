-module(day5).

-compile(export_all).

main(File) ->
    {ok, RawData} = file:read_file(File),
    Data = [ begin
                 [From, To] = binary:split(Line, <<" -> ">>),
                 [X1b, Y1b] = binary:split(From, <<",">>),
                 [X2b, Y2b] = binary:split(To,   <<",">>),
                 {{binary_to_integer(X1b), binary_to_integer(Y1b)},
                  {binary_to_integer(X2b), binary_to_integer(Y2b)}}
             end || Line <- binary:split(RawData, <<"\n">>, [global, trim]) ],
    io:format("part 1: ~p~n", [solve1(Data)]),
    io:format("part 2: ~p~n", [solve2(Data)]),
    ok.

solve1(Data) ->
    count_overlap([ L || Coords <- Data,
                              L <- to_straight_line(Coords) ]).

count_overlap(Lines) ->
    Map = lists:foldl(fun update_counter/2, #{}, Lines),
    length([ X || X <- maps:values(Map), X >= 2 ]).

update_counter(Coord, Acc) ->
    Fun = fun(V) -> V + 1 end,
    maps:update_with(Coord, Fun, 1, Acc).

to_straight_line({{X1, Y1}, {X2, Y2}}) when X1 =:= X2 ->
    [ {X1, Y} || Y <- from_to(Y1, Y2) ];
to_straight_line({{X1, Y1}, {X2, Y2}}) when Y1 =:= Y2 ->
    [ {X, Y1} || X <- from_to(X1, X2) ];
to_straight_line(_) ->
    [].

from_to(A, B) when B > A -> lists:seq(A, B);
from_to(A, B)            -> lists:seq(A, B, -1).

solve2(Data) ->
    count_overlap([ L || Coords <- Data,
                      L <- to_straight_line(Coords)
                        ++ to_diagonal_line(Coords) ]).

to_diagonal_line({{X1, Y1}, {X2, Y2}}) when abs(X1 - X2) =:= abs(Y1 - Y2) ->
    lists:zip(from_to(X1, X2), from_to(Y1, Y2));
to_diagonal_line(_) ->
    [].