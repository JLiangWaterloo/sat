#!/bin/bash          

for line in $(cat $1);
do
	echo "Processing $line"
	echo "File: $line" >> $2
	cat ../$line | ../Haskell/Graph variable > graph.tmp
	./../Snap/examples/community/community -i:graph.tmp -o:communities.tmp
	cat communities.tmp | grep "^#" >> $2
	./Minipure/binary/minipure -cpu-lim=3600 ../$line communities.tmp >> $2
	rm graph.tmp
 	rm communities.tmp
done
