#!/bin/bash

for file in $(ls *.dat)
do

    gnuplot << EOF
        set xlabel "N° of communities"
        set ylabel "Usage"
        set term png
        set output "${file}_usage.png"
        plot "${file}" using 1:2
EOF

done
