set key inside left top vertical Right noreverse enhanced autotitles box linetype -1 linewidth 1.000
set samples 50, 50
set title "Plot of Modularity vs CPU Time" 
set title  offset character 0, 0, 0 font ",20" norotate
set xlabel "Modularity"
set ylabel "CPU Time(s)"
set yrange [0:1]
plot "output/plotData.txt" using 1:2 with lines
