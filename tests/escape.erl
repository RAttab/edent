-module(escape).

-export([
        literal/0,
        string/0
]).

literal() ->
    [
        $\b,
        $\d,
        $\e,
        $\f,
        $\n,
        $\r,
        $\s,
        $\t,
        $\v,
        $\',
        $\",
        $\\
    ].

string() ->
    [
        "\b",
        "\d",
        "\e",
        "\f",
        "\n",
        "\r",
        "\s",
        "\t",
        "\v",
        "\'",
        "\"",
        "\\"
    ].
