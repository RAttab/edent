-module(macro).

-export([
        spec_test/3,
        spec_when_test/1,
        spec_lambda_test/2
    ]).

-record(record_test, {
        a,
        b
        =
            ok,
        c
        ::
            any(),
        d
        =
            ok
        ::
            any(),
        e
    }).

-type type_test()
::
    [
        #record_test{}
    ]
    |
    ok
    .

-spec spec_test(
    any()
    |
    ok,
    Name
    ::
        type_test()
        ,
    any()
    |
    ok
)
->
    ok
    |
    error
    .

spec_test(_, _, _) ->
    ok.

-spec spec_when_test(
    Name
)
->
    ok
when
    Name
    ::
        ok
        .

spec_when_test(_) ->
    ok.

-spec spec_lambda_test(
    Name,
    fun
        (
        (
            X
        )
        ->
            ok
    )
)
->
    ok
when
    Name
    ::
        fun
            (
            (
                X
            )
            ->
                ok
        )
        .

spec_lambda_test(_, _) ->
    ok.
