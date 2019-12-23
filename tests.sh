#! /usr/bin/env bash

set -o errexit -o nounset -o pipefail

declare -a TESTS
TESTS=(
    basics
#    hell # known bad cases
    block_if
    block_try
    block_case
    block_receive )

cd tests

for TEST in "${TESTS[@]}"; do
    input="${TEST}.erl"
    output="${TEST}.out.erl"

    printf "%0.1s" ={1..100}
    printf "\nTEST: %s\n\n" "$TEST"

    erlc "$input"
    ../edent "$input" "$output"
    diff "$input" "$output"
done
