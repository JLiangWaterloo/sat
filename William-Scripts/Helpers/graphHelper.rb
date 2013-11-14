#!/usr/bin/env ruby
load 'Helpers/graphConstructor.rb'

graph = GraphConstructor.new(ARGV[1], ARGV[2], ARGV[3], ARGV[4], ARGV[5])
graph.work(ARGV[0])
graph.finish()
