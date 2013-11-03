CC=gcc
CFLAGS=-std=c99 -g -Wall -O3

all: vm

vm: engine.o fail.o loader.o main.o memory_copying_gc.o

engine.o: src/engine.c

fail.o: src/fail.c

loader.o: src/loader.c

main.o: src/main.c

memory_copying_gc.o: src/memory_copying_gc.c

.PHONY:
clean:
	rm -rf *.o vm
