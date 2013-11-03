#!/bin/bash

srcs=(src/engine.c \
    src/fail.c \
    src/loader.c \
    src/main.c \
    src/memory_${memory_module:-copying_gc}.c)

mkdir -p bin
gcc -o bin/vm -std=c99 -g -Wall -O3 ${srcs[*]}
