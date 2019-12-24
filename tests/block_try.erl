-module(block_try).

-export([
        expr_test/1,
        catch_test/1,
        complex_test/1
    ]).


expr_test(X) ->
    catch X,
    ok.

catch_test(X) ->
    try
        X
    catch
        _:_:_ ->
            ok;
        _ ->
            ok
    end,
    ok.

complex_test(X) ->
    try
        X
    of
        ok ->
            ok;
        X when
            is_atom(X)
        ->
            X;
        _ ->
            X
    catch
        _:_:_ ->
            ok;
        _ ->
            ok
    after
        ok
    end,

    ok.
