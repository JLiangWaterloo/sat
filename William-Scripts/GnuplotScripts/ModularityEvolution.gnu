set key inside left top vertical Right noreverse enhanced autotitles box linetype -1 linewidth 1.000
set samples 50, 50
set title "Modularity Evolution" 
set title  offset character 0, 0, 0 font ",20" norotate
set xlabel "Dump"
set ylabel "Modularity"
plot "output/modularityEvolutionData.txt" using 1 title 'Community' with lines
