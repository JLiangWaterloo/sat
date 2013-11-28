#!/bin/bash        

SET=$(cat $1 | grep -v "^#")

for file in $SET;
do
	echo "File: $file" >> $2
	cat ../Benchmarks/$file | ../Haskell/Graph variable | ../Snap/examples/community/community -i:/dev/stdin -o:communities.tmp
	./CSat/CoMinipure/binary/minipure ../Benchmarks/$file communities.tmp | egrep "^c|^s" >> $2
 	rm communities.tmp
done
