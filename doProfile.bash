#!/bin/bash

set -e

profDir=./profiles

rm -f *.hi
rm -f *.o

ghc -O2 -prof -fprof-auto main.hs
./main tests/perf/while.blah +RTS -p

profs="$(ls -1 -c "$profDir")"
lastProfIndex="${profs%%$'.prof*'}"
newProfIndex="$((lastProfIndex + 1))"

mv main.prof "$profDir/$newProfIndex.prof"
