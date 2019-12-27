-module(block_lambda).

-export([
        basic_test/0,
        multi_test/0,
        when_test/0,
        param_test/0,
        reference_test/0
    ]).

basic_test() ->
    fun
        ()
        ->
            ok
    end.

multi_test() ->
    fun
        (ok)
        ->
            ok
            ;
        (_)
        ->
            ok
    end.

when_test() ->
    fun
        (X)
        when
            is_atom(X)
        ->
            ok
            ;
        (_)
        ->
            ok
    end.

param_test() ->
    fun
        (
            ok,
            {
                _Y,
                {
                    _Z
                }
            }
        )
        ->
            ok
            ;
        (
            _X,
            {
                _Y,
                {
                    _Z
                }}
        )
        ->
            ok
    end.

reference_test() ->
    fun param_test/0,
    fun () -> ok end (),
    ok.
