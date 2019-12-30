-module(basics).
-include("include.hrl").

-export([
        fun_test/1,
        atom_test/0,
        send_test/1,
        multiline_string_test/0
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

send_test(X) ->
    X
    !
        ok.

multiline_string_test() ->
    "abc
     def".
