#!/bin/sh

if [ "$1" = "demo" ]; then
	scala -cp ./bin "whilelang.FormulasDemo"
else
	scala -cp ./bin "whilelang.Main" $@
fi
