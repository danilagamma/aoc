-module(day2).

-compile(export_all).

main(File) ->
    {ok, RawData} = file:read_file(File),
    Data = [ N || N <- binary:split(RawData, <<"\n">>, [global, trim]) ],
    io:format("part 1: ~p~n", [solve1(Data)]),
    io:format("part 2: ~p~n", [solve2(Data)]).

solve1(Data) ->
    {X, Y} = lists:foldl(fun fold1/2, {0, 0}, Data),
    X * Y.

fold1(String, {X, Y}) ->
    [Direction, ValueB] = binary:split(String, <<" ">>),
    Value = binary_to_integer(ValueB),
    case Direction of
        <<"forward">> -> {X + Value, Y};
        <<"down">>    -> {X,         Y + Value};
        <<"up">>      -> {X,         Y - Value}
    end.

solve2(Data) ->
    {X, Y, _} = lists:foldl(fun fold2/2, {0, 0, 0}, Data),
    X * Y.

fold2(String, {X, Y, Z}) ->
    [Direction, ValueB] = binary:split(String, <<" ">>),
    Value = binary_to_integer(ValueB),
    case Direction of
        <<"forward">> -> {X + Value, Y + (Value * Z), Z};
        <<"down">>    -> {X,         Y,               Z + Value};
        <<"up">>      -> {X,         Y,               Z - Value}
    end.