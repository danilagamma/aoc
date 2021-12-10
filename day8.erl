-module(day8).

-compile(export_all).

main(File) ->
    {ok, RawData} = file:read_file(File),
    Data = [ [ [ ordsets:from_list(binary_to_list(N))
                 || N <- binary:split(L,  <<" ">>, [global, trim]) ]
               || L <- binary:split(Line, <<" | ">>, [trim]) ]
             || Line <- binary:split(RawData, <<"\n">>, [global, trim]) ],
    io:format("part 1: ~p~n", [solve1(Data)]),
    io:format("part 2: ~p~n", [solve2(Data)]),
    ok.

solve1(Data) ->
    length([ O || [_, Output] <- Data, O <- Output, lists:member(length(O), [2, 3, 4, 7]) ]).

solve2(Data) ->
    lists:sum([ solve_line(Input, Output) || [Input, Output] <- Data ]).

%% 1 -> "cf"
%% 7 -> "acf"
%% 4 -> "bcdf"
%% 2 -> "acdeg"
%% 3 -> "acdfg"
%% 5 -> "abdfg"
%% 0 -> "abcefg"
%% 6 -> "abdefg"
%% 9 -> "abcdfg"
%% 8 -> "abcdefg"

solve_line(Input, Output) ->
    [One]   = find_len(Input, 2),
    [Four]  = find_len(Input, 4),
    [Seven] = find_len(Input, 3),
    [Eight] = find_len(Input, 7),
    [Three] = [ N || N <- find_len(Input, 5), length(ordsets:intersection(N, One)) =:= 2 ],
    [Nine]  = [ N || N <- find_len(Input, 6), length(ordsets:subtract(N, Three))   =:= 1 ],
    ZeroSix = [ N || N <- find_len(Input, 6), N =/= Nine ],
    [Zero]  = [ N || N <- ZeroSix,            length(ordsets:intersection(N, One)) =:= 2 ],
    [Six]   = ZeroSix -- [Zero],
    TwoFive = [ N || N <- find_len(Input, 5), N =/= Three ],
    [Five]  = [ N || N <- TwoFive,            length(ordsets:intersection(N, Four)) =:= 3 ],
    [Two]   = TwoFive -- [Five],
    Map = #{Zero  => $0, One   => $1, Two   => $2,
            Three => $3, Four  => $4, Five  => $5,
            Six   => $6, Seven => $7, Eight => $8,
            Nine  => $9},
    list_to_integer([ maps:get(O, Map) || O <- Output ]).

find_len(List, Len) ->
    [ L || L <- List, length(L) =:= Len ].