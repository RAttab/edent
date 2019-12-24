#! /usr/bin/env bash

set -o errexit -o nounset -o pipefail

declare -a TESTS
TESTS=(
    basics
    # hell # known bad cases
    expr
    macro
    escape
    block_fun
    block_try
    block_case
    block_misc
    block_lambda
    block_receive
    edent )

if [[ $# -gt 0 ]]; then
    TESTS=( $1 )
fi

for TEST in "${TESTS[@]}"; do
    input="$(pwd)/tests/${TEST}.erl"
    output="$(pwd)/tests/${TEST}.out.erl"

    printf "%0.1s" ={1..100}
    printf "\nTEST: %s\n\n" "$TEST"

    if [[ "$TEST" != "edent" ]]; then
        (cd /tmp && erlc "$input")
    fi

    ./edent "$input" "$output"
    diff "$input" "$output"
done
