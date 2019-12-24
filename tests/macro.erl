-module(macro).

-export([
        spec_basics/2
    ]).

-record(rec_basic, {
        a = {} :: any(),
        b = [] :: any()
    }).

-type type_basic() :: #rec_basic{}.

-spec spec_basics(
    type_basic(),
    term())
->
    ok |
    error.

spec_basics(_, _) ->
    ok.
