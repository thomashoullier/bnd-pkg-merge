# Gnuplot script to display the results of mem-monitor.sh
set key autotitle columnhead
set datafile separator " "
set grid
set xlabel "Time (ms)"
set ylabel "Memory (KB)"

plot '104777e56f.txt' w linespoints title "104777e56f",\
     '12999876e1-halved-chains.txt' w linespoints title "half chains",\
     '9bacfcc9f1.txt' w linespoints title "garbage collected",\
     '../test.txt' w linespoints title "current"
