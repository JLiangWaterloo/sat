#!/usr/bin/env ruby
require 'diffy'
system 'rm -fr RegressionTestSuite/diff'
system 'mkdir -p RegressionTestSuite/diff'

#
# Test CommunityOutputOnlyModularity
#
# Plan: Compare output of fiasco to original output using diff
#
puts "--- Testing CommunityOutputOnlyModularity ---"
output = `cat ../Benchmarks/fiasco.dimacs | ./CommunityOutputOnlyModularity`
file = File.open("RegressionTestSuite/originals/communityOutputOnlyModularity/output.txt")
original = file.readlines[0]
file.close

if original == output
  puts "  No change"
else
  puts "  There was a DIFFERENCE! Outputting to diff folder"
  file = File.open("RegressionTestSuite/diff/communityOutputOnlyModularity_diff.txt", "w")
  file.write(Diffy::Diff.new(original, output))
  file.close
end


#
# Test CommunityExt
#
# Plan: Compare output of fiasco to original output using diff
#
puts "--- Testing CommunityExt ---"
output = `./CommunityExt ../Benchmarks/fiasco.dimacs`
file = File.open("RegressionTestSuite/originals/communityExt/output.txt")
original = file.read
file.close

if original == output
  puts "  No change"
else
  puts "  There was a DIFFERENCE! Outputting to diff folder"
  file = File.open("RegressionTestSuite/diff/communityExt_diff.txt", "w")
  file.write(Diffy::Diff.new(original, output))
  file.close
end


#
# Test CommunityGrapher
#
# Plan: Compare graph, communityMapping and graphviz input file to originals 
#       using diff for fiasco for both graphviz and ubigraph
#
puts "--- Testing CommunityGrapher ---"
system './CommunityGrapher ../Benchmarks/fiasco.dimacs graphviz n jpg'
Dir.foreach('RegressionTestSuite/originals/communityGrapher/*.dot') do |file|
  
end


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
