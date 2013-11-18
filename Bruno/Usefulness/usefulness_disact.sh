#!/bin/bash          

SET=$(cat $1 | grep -v "^#")

for file in $SET;
do
	echo "File: $file" >> $2
	../Minipure/binary/minipure -dis-act ../Benchmarks/$file >> $2
done
