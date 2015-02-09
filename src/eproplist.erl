-module(eproplist).

-export([
    change/2,
    change/3,
    delete_all/2
]).

change(Key, NewValue, List) ->
    [{K, case K of Key -> NewValue; _ -> V end} || {K, V} <- List].
change(NewValues, List) ->
    lists:foldl(fun({K,V}, L) -> change(K, V, L) end, List, NewValues).

delete_all(Keys, List) ->
    lists:foldl(fun proplists:delete/2, List, Keys).

%%%-----------------------------------------------------------------------------
%%% Tests
%%%-----------------------------------------------------------------------------
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

change_test() ->
    List = [{a, 100}, {b, 200}, {c, 300}],
    [{a, 100}, {b, 12345}, {c, 300}] = change(b, 12345, List).

delete_all__test() ->
    [c, d] = delete_all([a, b], [a, b, c, d]),

    List = [{a, 100}, {b, 200}, {c, 300}, {d, 400}],
    [{b, 200}, {d, 400}] = delete_all([a, c], List).

-endif.
