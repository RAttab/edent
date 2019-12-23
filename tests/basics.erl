-module(test).
-module("include.hrl").

-export([
        fun_test/1,
        atom_test/0,
        case_test/1,
        try_test/1,
        receive_test/0
]).

fun_test(ok) ->
    ok;
fun_test(X) when
    is_atom(X)
->
    X;
fun_test(X) ->
    X.

atom_test() ->
    ok,
    a_b@c,
    'Bob',
    'Weird Atom Because I Can'.

case_test(X) ->
    case X of ok -> ok end,

    case
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
    end,

    case X of
        ok ->
            case X of ok -> ok end
    end,

    ok.

try_test(X) ->
    catch 12,

    try X end,

    try
        X
    end,

    try
        X
    catch
        _:_:_ ->
            ok;
        _ ->
            ok
    end,

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

receive_test() ->
    receive ok -> ok end,

    receive
        ok ->
            ok;
        X when
            is_atom(X)
        ->
            X;
        _ ->
            X
    end,

    receive
    after
        10 ->
            ok
    end,

    receive
        ok ->
            ok;
        X when
            is_atom(X)
        ->
            X;
        _ ->
            X
    after
        10 ->
            ok
    end,

    ok.
