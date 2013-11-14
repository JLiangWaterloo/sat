#!/usr/bin/env ruby

#
# Test CommunityOutputOnlyModularity
#
# Plan: Compare output of fiasco to original output using diff
#
output = `cat ../ks/fiasco.dimacs | ./CommunityOutputOnlyModularity`

#
# Test CommunityExt
#
# Plan: Compare output of fiasco to original output using diff
#

#
# Test CommunityGrapher
#
# Plan: Compare graph, communityMapping and graphviz input file to originals 
#       using diff for fiasco for both graphviz and ubigraph
#

#
# Test CommunityEvolution
#
# Plan: Compare graph, communityMapping and graphviz input file to originals 
#       using diff for fiasco for every step of the evolution for graphviz and 
#       ubigraph. And compare the graph data files for both edgeplot and modularity
#

#
# Test PlotCommunityAnalysis
#
# Plan: Compare the plot data to the originals
#
