set key inside left top vertical Right noreverse enhanced autotitles box linetype -1 linewidth 1.000
set samples 50, 50
set title "Community vs intercommunity edges that are removed" 
set title  offset character 0, 0, 0 font ",20" norotate
set xlabel "Dump"
set ylabel "Edges"
plot "output/removedEdgeTypeCountData.txt" using 2 title 'Community' with lines, \
     "output/removedEdgeTypeCountData.txt" using 3 title 'Interommunity' with lines
