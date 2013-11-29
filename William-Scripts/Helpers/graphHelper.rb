#!/usr/bin/env ruby
DIR_NAME_GRAPH_HELPER = File.expand_path File.dirname(__FILE__)
load DIR_NAME_GRAPH_HELPER + '/graphConstructor.rb'

graph = GraphConstructor.new(ARGV[1], ARGV[2], ARGV[3], ARGV[4], ARGV[5])
graph.work(Dir.pwd + "/" +  ARGV[0])
graph.finish()
