-module(econv).

-export([
    to_list/1
]).

to_list(V) when is_integer(V) -> integer_to_list(V);
to_list(V) when is_atom(V) -> atom_to_list(V);
to_list(V) when is_binary(V) -> binary_to_list(V);
to_list(V) when is_tuple(V) -> tuple_to_list(V);
to_list(V) -> V.

%%%-----------------------------------------------------------------------------
%%% Tests
%%%-----------------------------------------------------------------------------
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

to_list_test() ->
    "foo" = to_list(foo),
    "123" = to_list(123),
    "foo" = to_list(foo),
    "foo" = to_list(<<"foo">>),
    1.123 = to_list(1.123).

-endif.
