#!/bin/bash

good_tests=( 
    "if if true then false else true then true else false"
    "if false then 1 else 2"
    "if true then 1 else 2"
    "true"
    "false"
    "3"
    "iszero 0"
    "iszero pred 0"
    "iszero succ 0"
    "pred 3"
    "succ 4"
    "if iszero pred pred 2 then if iszero 0 then true else false else false"
    )

invalid_tests=(
    "if true then if iszero succ pred succ pred succ pred pred 1 then true else false else false"
    "if if if iszero pred pred if iszero succ pred succ pred pred succ succ pred 0 then succ 1 else false then iszero 0 else true then true else true then if false then true else succ succ pred succ false else 4"
    "pred pred pred pred 0"
    "pred succ pred pred 0"
    "iszero succ pred pred pred pred pred 0"
    "iszero succ pred pred succ succ succ succ pred 0"
    "iszero succ pred pred pred 0"
    "if false then 1 else succ false"
    "pred succ succ succ false"
    "if iszero pred pred 2 then if iszero 0 then iszero pred succ succ succ true else false else false"
    "if iszero pred pred 2 then if iszero 0 then iszero pred succ succ succ 0 else false else false"
    "if iszero if true then true else false then true else true"
    "pred succ succ succ if true then false else false"
    "iszero succ pred pred iszero 0"
    "if iszero succ pred pred true then true else false"
    "iszero 0"
    "if false then 5 else succ 4"
    "if iszero succ false then true else false"
    "if true then if succ pred succ pred succ pred pred 0 then true else false else false"
    "if false then false else if 0 then true else false"
    "iszero if false then false else true"
    "pred succ succ pred true"
    "pred succ succ true"
    "succ false"
    "truf"
    "fals"
    "0.25"
    )

cd classes

TEST_GOOD=false
TEST_INVALID=true

if [ $TEST_GOOD = true ]; then
    # Test the good tests
    for i in "${good_tests[@]}"
    do
        echo ''
        echo "=== Running the test ||| " $i " ||| ==="
        scala fos.Arithmetic "$i"
        read text
    done
fi

if [ $TEST_INVALID = true ]; then
    # Test the invalid tests
    for i in "${invalid_tests[@]}"
    do
        echo ''
        echo "=== Running the test ||| " $i " ||| ==="
        scala fos.Arithmetic "$i"
        read text
    done
fi

