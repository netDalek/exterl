-module(emath).

-export([
    ceil/1
    ]).

ceil(X) ->
    T = erlang:trunc(X),
    case (X - T) of
        Neg when Neg < 0 -> T;
        Pos when Pos > 0 -> T + 1;
        _ -> T
    end.

%%%-----------------------------------------------------------------------------
%%% Tests
%%%-----------------------------------------------------------------------------
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

ceil_test() ->
    1 = ceil(1),
    4 = ceil(3.1),
    4 = ceil(3.9).

-endif.
