-module(day17).

-compile(export_all).

main(File) ->
    {ok, RawData} = file:read_file(File),
    {match, RawTarget} = re:run(RawData, <<"([-\\d]+)">>, [{capture, all_but_first, list}, global]),
    Target = list_to_tuple([ list_to_integer(X) || [X] <- RawTarget ]),
    io:format("part 1: ~p~n", [solve1(Target)]),
    io:format("part 2: ~p~n", [solve2(Target)]).

solve1(Target) ->
    lists:max(heights(Target)).

solve2(Target) ->
    length(heights(Target)).

heights({_MinX, MaxX, MinY, _MaxY} = Target) ->
    [ H || X <- lists:seq(1, MaxX),
           Y <- lists:seq(MinY, abs(MinY)),
           (H = iter_steps({X, Y}, Target)) =/= false ].

iter_steps(V, Target) ->
    iter_steps(V, Target, {0, 0}, 0).

iter_steps(V, Target, {_, Y} = C, H) ->
    case position(C, Target) of
        hit      -> H;
        over     -> false;
        continue ->
            {NewV, NewC} = step(V, C),
            iter_steps(NewV, Target, NewC, max(Y, H))
    end.

step({Dx, Dy}, {X, Y}) ->
    {{max(0, Dx - 1), Dy - 1}, {X + Dx, Y + Dy}}.

position({X, Y}, { MinX, MaxX, MinY,  MaxY}) when X >= MinX, X =< MaxX, Y >= MinY, Y =< MaxY -> hit;
position({X, Y}, {_MinX, MaxX, MinY, _MaxY}) when X  > MaxX; Y  < MinY                       -> over;
position(_XY,    _Target)                                                                    -> continue.
