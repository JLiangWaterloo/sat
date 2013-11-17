#!/bin/bash          

echo "Result of BCP_Community.sh" > $2
for line in $(cat $1);
do
	echo "Processing $line"
	echo "File: $line" >> $2
	cat ../$line | ../Haskell/Graph variable > graph.tmp
	./../Snap/examples/community/community -i:graph.tmp -o:communities.tmp
	cat communities.tmp | grep "^#" >> $2
	./CSat/CoMinipure/binary/minipure -cpu-lim=3600 ../$line communities.tmp > result.tmp
	cat result.tmp | egrep "^c|^s|^a" >> $2
	rm result.tmp
	rm graph.tmp
 	rm communities.tmp
done
