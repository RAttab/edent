%% Because the lexer in erlang removes all the escape sequences and
%% just gives you the raw characters, it's strictly not possible to
%% determine what the original character in a string was.
%%
%% This file also serves as documentation of the choices we made with
%% what will be escaped and what won't.
%%
%% TODO: hex and octal encodings

-module(escape).

-export([
        literal/0,
        string/0
]).

literal() ->
    [
        $A,
        $1,
        $(,
        $',
        $",

        $\b,
        $\d,
        $\e,
        $\f,
        $\n,
        $\r,
        $\s,
        $\t,
        $\v,
        $\\,

        $\^A,
        $\^C,
        $\^W,
        $\^Z
    ].

string() ->
    [
        " ",
        "'",

        "\b",
        "\d",
        "\e",
        "\f",
        "\n",
        "\r",
        "\t",
        "\v",
        "\"",
        "\\",

        "\^A",
        "\^C",
        "\^W",
        "\^Z"
    ].
