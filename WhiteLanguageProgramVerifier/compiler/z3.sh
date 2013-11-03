#!/bin/sh
# faking accepting input
cat > z3/prog.tmp
wine z3/z3.exe -smt z3/prog.tmp 2>/dev/null
rm z3/prog.tmp

