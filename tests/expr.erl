-module(expr).

-export([
        eq_test/1
    ]).

eq_test(X) ->
    _ =
        ok,
    _ =
        ok =
            X,

    _ =
        {
            X,
            ok
        },

    _ = [X, ok],
    _ = X(ok, ok),
    _ = <<$a, $b>>,

    case
        ok =
            X
    of
        _ =
            _
        ->
            _ =
                X
    end,

    try
        _ =
            X
    catch
        _ -> ok
    end,

    try
        _ =
            X
    after
        ok
    end.
