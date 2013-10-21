sat
===

Install
-------
```bash
git clone https://github.com/JLiangWaterloo/sat.git
cd sat
make
```
Note that this will download and decompress a very large file from the SAT competition server.

Tools
-----
##### Haskell/BCP
Input: A dimacs file.

Output: A dimacs file after performing boolean constraint propagation (unit resolution).

##### Haskell/Cmty \<vars> \<clauses> \<cmtys> \<q>
Input: Arguments.

Output: A random dimacs file with \<vars> variables, \<clauses> clauses, and \<cmtys> communities. The strength of these communities is configured by \<q>, a value between 0 and 1. The higher the \<q>, the stronger the community.

##### Haskell/Graph \<clause or variable or literal>
Input: Arguments.

Output: A graph in a format that is parsable by the SNAP library. If the argument is "clause" then clauses are nodes and variables edges. If the argument is "variable" then variables are nodes and clauses are edges. If the argument is "literal" then literals are nodes and clauses are edges, with the addition that there is an extra edge between every positive and negative literals with the same variable.

##### Haskell/Shuffle
Input: A dimacs file.

Output: A dimacs file with the variables randomly renumbered.

##### Community
Input: A dimacs file.

Output: 6 lines of statistics about the dimacs: # of nodes, # of communities, # network modularity, size of the largest community, size of the average community, the number of communities that are larger than average.

##### CommunityGrapher
Requires: GraphViz (http://www.graphviz.org/pub/graphviz/stable/ubuntu/ub13.04/x86_64/graphviz_2.34.0-1~raring_amd64.deb) and Ruby

Input: A dimacs file (optional).

Output: Two GraphViz graphs which show the mapping of clauses or variables as chosen and the mapping of either to their community respectively. If no file was given, the script will prompt to user to enter the desired number of variables, clauses, communities, and quality of communities. They will both have to then choose to either represent nodes as clauses and edges as variables or vice versa. Will also have to choose to either build a detailed version of graph which uses a lot more time or not. Now has the option to use UbiGraph as well. To use UbiGraph, its server must be started first. To do that, run the following command from the 'sat' folder: UbiGraph-alpha-0.2.4-Linux64-Ubuntu-8.04/bin/ubigraph_server &.

##### CommunityGrapher
Requires: GraphViz (http://www.graphviz.org/pub/graphviz/stable/ubuntu/ub13.04/x86_64/graphviz_2.34.0-1~raring_amd64.deb), Ruby and ImageMagick

Input: A zip file with an alphabetically sorted set of dimacs files or cnf (untested)

Output: Will generate a GIF of going from the first file to the last.

##### PlotCommunityAnalysis
Requires: GnuPlot

Input: Will be prompted for the number of variables, clauses, and communities.

Output: A plot of the performance of MiniSAT while varying Q from 0 to 1 by increments of 0.01.

Example
-------
```bash
cat Benchmarks/toybox.dimacs | ./Community
```
Will output
```
181
44
0.826756
25
4.11
10
```
What's interesting is that it has a strong modularity (0.826756) and the largest community (44) is a small part of the overall problem (181).
