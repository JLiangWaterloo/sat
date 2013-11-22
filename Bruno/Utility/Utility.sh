#!/bin/bash     

SET=$(cat $1 | grep -v "^#")


for file in $SET;
do
	cat ../Benchmarks/$file | ../Haskell/Graph variable | ../Snap/examples/community/community -i:/dev/stdin -o:communities.tmp
	./CSat/CoMinipure/binary/minipure ../Benchmarks/$file communities.tmp Utility/${file//"/"/"_"}.dat
 	rm communities.tmp
done
