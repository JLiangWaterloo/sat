#!/bin/bash

cat $1 | ./../Haskell/Graph variable | ./../Snap/examples/community/community -i:/dev/stdin -o:$2

