#!/usr/bin/env ruby
load 'Helpers/rubigraph.mine'
load 'Helpers/RubiGraphBuilder.rb'

Rubigraph.init
Rubigraph.clear
rubiGraph = RubiGraphBuilder.new()
rubiGraph.buildOriginalGraph()
