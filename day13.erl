-module(day13).

-compile(export_all).

main(File) ->
    {ok, RawData} = file:read_file(File),
    [RawDots, RawFold] = binary:split(RawData, <<"\n\n">>),
    Dots = lists:sort([ begin
                            [X, Y] = binary:split(Line, <<",">>),
                            {binary_to_integer(X), binary_to_integer(Y)}
                        end || Line <- binary:split(RawDots, <<"\n">>, [global, trim]) ]),
    Fold = [ begin
                 [Instruction, V] = binary:split(Line, <<"=">>, [global, trim]),
                 {Instruction, binary_to_integer(V)}
             end || Line <- binary:split(RawFold, <<"\n">>, [global, trim]) ],
    io:format("part 1: ~p~n", [solve1(Dots, Fold)]),
    io:format("part 2: ~p~n", [solve2(Dots, Fold)]).

solve1(Map, Fold) ->
    length(fold(hd(Fold), Map)).

solve2(Map, Fold) ->
    print(lists:foldl(fun fold/2, Map, Fold)).

fold(Instruction, Map) ->
    lists:usort([ fold_dot(Dot, Instruction) || Dot <- Map ]).

fold_dot({X, Y}, {<<"fold along y">>, V}) when Y > V -> {X, 2 * V - Y};
fold_dot({X, Y}, {<<"fold along x">>, V}) when X > V -> {2 * V - X, Y};
fold_dot({X, Y}, _)                                  -> {X, Y}.

print(Map) ->
    [ print(Map, Y) || Y <- lists:usort([ Y || {_, Y} <- Map ]) ],
    ok.

print(Map, Y) ->
    Dots = [ X || {X, Y0} <- Map, Y0 =:= Y ],
    [ case lists:member(X, Dots) of
          true  -> io:format("#");
          false -> io:format(" ")
      end || X <- lists:seq(0, lists:max(Dots)) ],
    io:format("~n").