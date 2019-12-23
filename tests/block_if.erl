-module(block_if).

-export([
        basic_test/1
]).

basic_test(X) ->
    if
        X ->
            ok;
        true ->
            ok
    end,
    ok.
