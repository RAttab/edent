-module(macro).

-export([
        spec_test/2,
        spec_when_test/1
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
)
->
    ok
    |
    error
    .

spec_test(_, _) ->
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
