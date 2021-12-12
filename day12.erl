-module(day12).

-compile(export_all).

main(File) ->
    {ok, RawData} = file:read_file(File),
    Data = [ binary:split(Line, <<"-">>)
             || Line <- binary:split(RawData, <<"\n">>, [global, trim]) ],
    Map = load(Data),
    io:format("part 1: ~p~n", [solve1(Map)]),
    io:format("part 2: ~p~n", [solve2(Map)]).

load(Data) ->
    lists:foldl(fun ([F, T], A) ->
                        A0 = maps:update_with(F, fun(V) -> [T|V] end, [T], A),
                        maps:update_with(T, fun(V) -> [F|V] end, [F], A0)
                end,
                #{},
                Data).

solve1(Graph) ->
    length(paths(Graph, false)).

solve2(Graph) ->
    length(paths(Graph, true)).

paths(Graph, HaveTime) ->
    paths(maps:get(<<"start">>, Graph), Graph, HaveTime, #{}, [<<"start">>]).

paths([], _Graph, _HaveTime, _Visited, _PathAcc) ->
    [];
paths([<<"start">>|Nodes], Graph, HaveTime, Visited, PathAcc) ->
    paths(Nodes, Graph, HaveTime, Visited, PathAcc);
paths([<<"end">> = Node|Nodes], Graph, HaveTime, Visited, PathAcc) ->
    [lists:reverse([Node|PathAcc])|paths(Nodes, Graph, HaveTime, Visited, PathAcc)];
paths([Node|Nodes], Graph, HaveTime, Visited, PathAcc)
  when is_map_key(Node, Visited), not HaveTime ->
    paths(Nodes, Graph, HaveTime, Visited, PathAcc);
paths([Node|Nodes], Graph, HaveTime, Visited, PathAcc) ->
    {NewHaveTime, NewVisited}
        = case string:titlecase(Node) of
              Node   -> {HaveTime,                                    Visited};
              _Other -> {HaveTime and not maps:is_key(Node, Visited), Visited#{Node => true}}
          end,
    paths(maps:get(Node, Graph), Graph, NewHaveTime, NewVisited, [Node|PathAcc])
 ++ paths(Nodes, Graph, HaveTime, Visited, PathAcc).