-module(block_case).

-export([
        basic_test/1,
        complex_test/1,
        nested_test/1
    ]).

basic_test(X) ->
    case
        X
    of
        ok
        ->
            ok
    end,
    ok.

complex_test(X) ->
    case
        X
    of
        ok
        ->
            ok
            ;
        X
        when
            is_atom(X)
        ->
            X
            ;
        _
        ->
            X
    end,
    ok.

nested_test(X) ->
    case X of
        ok ->
            case
                X
            of
                ok
                ->
                    ok
            end
            ;
        _
        ->
            ok
    end,
    ok.
