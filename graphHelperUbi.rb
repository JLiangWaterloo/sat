#!/usr/bin/env ruby
require 'rubigraph'
load 'RubiGraphBuilder.rb'

Rubigraph.init
Rubigraph.clear
rubiGraph = RubiGraphBuilder.new()
rubiGraph.buildOriginalGraph()
