-module(day16).

-compile(export_all).

main(File) ->
    {ok, RawData} = file:read_file(File),
    Binary = binary:decode_hex(RawData),
    io:format("part 1: ~p~n", [solve1(Binary)]),
    io:format("part 2: ~p~n", [solve2(Binary)]).

solve1(Data) ->
    {Parsed, _} = parse(Data),
    count_versions(Parsed).

parse(<<Version:3, 4:3, Packet/bits>>) ->
    {Value, Rest} = parse_literal(Packet, 0),
    {{Version, literal, Value}, Rest};
parse(<<Version:3, TypeId:3, 0:1, Len:15, Packet/bits>>) ->
    <<Part:Len/bits, Rest/bits>> = Packet,
    Packets = parse_subpacket(Part),
    {{Version, operator, TypeId, Packets}, Rest};
parse(<<Version:3, TypeId:3, 1:1, Num:11, Packet/bits>>) ->
    {Packets, Rest} = parse_subpacket(Packet, Num, []),
    {{Version, operator, TypeId, Packets}, Rest}.

parse_subpacket(<<>>)   -> [];
parse_subpacket(Packet) ->
    {Parsed, Rest} = parse(Packet),
    [Parsed|parse_subpacket(Rest)].

parse_subpacket(Packet, 0, Acc) -> {lists:reverse(Acc), Packet};
parse_subpacket(Packet, N, Acc) ->
    {Parsed, Rest} = parse(Packet),
    parse_subpacket(Rest, N - 1, [Parsed|Acc]).

parse_literal(<<1:1, Value:4, Rest/bits>>, Acc) ->
    parse_literal(Rest, Acc bsl 4 + Value);
parse_literal(<<0:1, Value:4, Rest/bits>>, Acc) ->
    {Acc bsl 4 + Value, Rest}.

count_versions({V, operator, _TypeId, Packets}) ->
    V + lists:sum([ count_versions(P) || P <- Packets ]);
count_versions({V, literal, _}) ->
    V.

solve2(Data) ->
    {Parsed, _} = parse(Data),
    calc(Parsed).

calc({_, literal, Value}) ->
    Value;
calc({_, operator, TypeId, Packets}) ->
    Values = lists:map(fun calc/1, Packets),
    case TypeId of
        0 -> lists:sum(Values);
        1 -> lists:foldl(fun erlang:'*'/2, 1, Values);
        2 -> lists:min(Values);
        3 -> lists:max(Values);
        5 ->
            case hd(Values) > hd(tl(Values)) of
                true  -> 1;
                false -> 0
            end;
        6 ->
            case hd(Values) < hd(tl(Values)) of
                true  -> 1;
                false -> 0
            end;
        7 ->
            case hd(Values) =:= hd(tl(Values)) of
                true  -> 1;
                false -> 0
            end
    end.
