#! /usr/bin/env bash

set -o errexit -o nounset -o pipefail

declare -a TESTS
TESTS=(
    basics
#    hell # known bad cases
    block_try
    block_case
    block_misc
    block_receive )

for TEST in "${TESTS[@]}"; do
    input="$(pwd)/tests/${TEST}.erl"
    output="$(pwd)/tests/${TEST}.out.erl"

    printf "%0.1s" ={1..100}
    printf "\nTEST: %s\n\n" "$TEST"

    (cd /tmp && erlc "$input")

    ./edent "$input" "$output"
    diff "$input" "$output"
done
