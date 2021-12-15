-module(day14).

-compile(export_all).

main(File) ->
    {ok, RawData} = file:read_file(File),
    [TemplateRaw, RulesRaw] = binary:split(RawData, <<"\n\n">>),
    Rules = [ begin
                  [From, To] = binary:split(R, <<" -> ">>),
                  {binary_to_list(From), hd(binary_to_list(To))}
              end || R <- binary:split(RulesRaw, <<"\n">>, [global, trim]) ],
    Template = binary_to_list(TemplateRaw),
    RulesMap = maps:from_list(Rules),
    io:format("part 1: ~p~n", [solve1(Template, RulesMap)]),
    io:format("part 2: ~p~n", [solve2(Template, RulesMap)]).

solve1(Template, Rules) ->
    NewTemplate = grow(Template, Rules, 10),
    Counts = maps:values(freq(NewTemplate)),
    lists:max(Counts) - lists:min(Counts).

grow(Template, _Rules, 0)    -> Template;
grow(Template,  Rules, Step) ->
    grow(grow(Template, Rules), Rules, Step - 1).

grow([A, B|Rest], Rules) ->
    [A, maps:get([A, B], Rules)|grow([B|Rest], Rules)];
grow([B], _) ->
    [B].

freq(List) ->
    lists:foldl(fun (K, Acc) -> incr_count(K, 1, Acc) end, #{}, List).

incr_count(Key, V, Map) ->
    Map#{Key => maps:get(Key, Map, 0) + V}.

solve2(Template, Rules) ->
    {_, CountMap} = grow_count(Template, Rules, 40),
    Counts = maps:values(CountMap),
    lists:max(Counts) - lists:min(Counts).

init_maps([B], PairMap, CountMap) ->
    {PairMap, incr_count(B, 1, CountMap)};
init_maps([A, B|Rest], PairMap, CountMap) ->
    init_maps([B|Rest],
              incr_count([A, B], 1, PairMap),
              incr_count(A,      1, CountMap)).

grow_count(Template, Rules, Steps) ->
    {PairMap, CountMap} = init_maps(Template, #{}, #{}),
    grow_count(PairMap, CountMap, Rules, Steps).

grow_count(PairMap, CountMap, _Rules, 0)    -> {PairMap, CountMap};
grow_count(PairMap, CountMap,  Rules, Step) ->
    {NewPairMap, NewCountMap}
        = maps:fold(fun (K, V, Acc) -> fold_pair(K, V, Rules, Acc) end,
                    {#{}, CountMap},
                    PairMap),
    grow_count(NewPairMap, NewCountMap, Rules, Step - 1).

fold_pair([A, B], V, Rules, {PairMap, CountMap}) ->
    C = maps:get([A, B], Rules),
    NewPairMap0 = incr_count([A, C], V, PairMap),
    NewPairMap  = incr_count([C, B], V, NewPairMap0),
    NewCountMap = incr_count(C,      V, CountMap),
    {NewPairMap, NewCountMap}.