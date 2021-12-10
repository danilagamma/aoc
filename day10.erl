-module(day10).

-compile(export_all).

main(File) ->
    {ok, RawData} = file:read_file(File),
    Data = [ binary_to_list(Line) || Line <- binary:split(RawData, <<"\n">>, [global, trim]) ],
    io:format("part 1: ~p~n", [solve1(Data)]),
    io:format("part 2: ~p~n", [solve2(Data)]).

solve1(Data) ->
    lists:sum([ score_corrupted(Char) || {corrupted, Char} <- read(Data) ]).

solve2(Data) ->
    Scores = [ score_incomplete(Chars) || {incomplete, Chars} <- read(Data) ],
    lists:nth(length(Scores) div 2 + 1, lists:sort(Scores)).

read(Data) ->
    [ read(Chars, []) || Chars <- Data ].

read([], Stack) ->
    {incomplete, Stack};
read([C|T], [C|R]) ->
    read(T, R);
read([C|T], Stack)
  when C =:= $(; C =:= $[;
       C =:= ${; C =:= $< ->
    Closing = case C of
                  $( -> $);
                  $[ -> $];
                  ${ -> $};
                  $< -> $>
              end,
    read(T, [Closing|Stack]);
read([C|_], _Stack) ->
    {corrupted, C}.

score_corrupted(C) ->
    case C of
        $) -> 3;
        $] -> 57;
        $} -> 1197;
        $> -> 25137
    end.

score_incomplete(Chars) ->
    lists:foldl(fun (C, Acc) -> 5 * Acc + score_incomplete_char(C) end, 0, Chars).

score_incomplete_char(C) ->
    case C of
        $) -> 1;
        $] -> 2;
        $} -> 3;
        $> -> 4
    end.