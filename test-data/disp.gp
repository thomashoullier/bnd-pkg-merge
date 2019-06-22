# Gnuplot script to display the results of mem-monitor.sh
set key autotitle columnhead
set datafile separator " "
set grid
set xlabel "Time (ms)"
set ylabel "Memory (KB)"

plot '104777e56f.txt' w linespoints title "asking pairs",\
     '12999876e1-halved-chains.txt' w linespoints title "half chains",\
     '9bacfcc9f1.txt' w linespoints title "garbage collected",\
     'a25427ab3b-closures.txt' w linespoints title "closures",\
     'a25427ab3b-agressive-gc.txt' w linespoints title "agressive gc",\
     '../test.txt' w linespoints title "current"
