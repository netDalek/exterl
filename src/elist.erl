-module(elist).

-export([
    find/2,
    first_defined/1,
    remove_undefined/1,
    all_defined/1,
    random/1,
    randomize/1,
    map_with_index/2,
    split/2,
    split_in_chunks/2,
    join/2,
    set_element/3,
    filtermap/2
]).

find(_Pred, []) -> undefined;
find(Pred, [Head|Rest]) ->
    case Pred(Head) of
        true -> Head;
        false -> find(Pred, Rest)
    end.

first_defined(List) ->
    find(fun(Val) -> Val =/= undefined end, List).

all_defined(List) ->
    lists:all(fun(V) -> V =/= undefined end, List).

remove_undefined(List) ->
    [Val || Val <- List, Val =/= undefined].

random([]) -> undefined;
random(List) -> lists:nth(random:uniform(length(List)), List).

randomize(List) ->
    [X || {_, X} <- lists:sort([{random:uniform(), N} || N <- List])].

map_with_index(Fun, List) ->
    lists:map(fun({Val, Index}) -> Fun(Val, Index) end, lists:zip(List, lists:seq(0, length(List) - 1))).

split_in_chunks(List, ChunksCount) ->
    split(List, emath:ceil(length(List) / ChunksCount)).

split(List, SliceSize) -> lists:reverse(split(List, SliceSize, [])).
split(List, SliceSize, Acc) when length(List) =< SliceSize ->
    [List|Acc];
split(List, SliceSize, Acc) ->
    {Chunk, Tail} = lists:split(SliceSize, List),
    split(Tail, SliceSize, [Chunk|Acc]).

join([], _Sep) ->
  "";
join([H|[]], _Sep) ->
  econv:to_list(H);
join([H|T], Sep) ->
  econv:to_list(H) ++ econv:to_list(Sep) ++ join(T, Sep).

set_element(List, Index, Value) ->
    lists:sublist(List, Index - 1) ++ [Value] ++ lists:nthtail(Index, List).

%filtermap definition for erlang less R16
filtermap(Fun, List1) ->
    lists:foldr(fun(Elem, Acc) ->
                       case Fun(Elem) of
                           false -> Acc;
                           true -> [Elem|Acc];
                           {true,Value} -> [Value|Acc]
                       end
                end, [], List1).

%%%-----------------------------------------------------------------------------
%%% Tests
%%%-----------------------------------------------------------------------------
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

first_defined_test() ->
    a = first_defined([undefined, a, b, c]),
    undefined = first_defined([]).

all_defined_test() ->
    true = all_defined([a, b, c]),
    false = all_defined([a, undefined, b]).

remove_undefined_test() ->
    [a, b] = remove_undefined([undefined, a, undefined, b]),
    [] = remove_undefined([]).

map_with_index_test() ->
    [{0, a}, {1, b}, {2, c}] = map_with_index(fun(Val, Index) -> {Index, Val} end, [a, b, c]).

split_in_chunks_test() ->
    [[1,2], [3,4]] = split_in_chunks([1,2,3,4], 2),
    [[1,2], [3,4], [5]] = split_in_chunks([1,2,3,4,5], 3).

split_test() ->
    [[1,2], [3,4]] = split([1,2,3,4], 2),
    [[1,2], [3,4], [5]] = split([1,2,3,4,5], 2).

join_test() ->
  "1,2,3" = join([1, 2, 3], ","),
  "1" = join([1], ","),
  "" = join([], ",").

-endif.
