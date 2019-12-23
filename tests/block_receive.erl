-module(block_receive).

-export([
        basic_test/0,
        match_test/0,
        after_test/0,
        complex_test/0
]).

basic_test() ->
    receive
        ok
        ->
            ok
    end,
    ok.

match_test() ->
    receive
        ok ->
            ok;
        X when
            is_atom(X)
        ->
            X;
        _ ->
            ok
    end,
    ok.

after_test() ->
    receive
    after
        10 ->
            ok
    end,
    ok.

complex_test() ->
    receive
        ok ->
            ok;
        X when
            is_atom(X)
        ->
            X;
        _ ->
            ok
    after
        10 ->
            ok
    end,
    ok.
