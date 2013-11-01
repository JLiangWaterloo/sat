#!/usr/bin/env ruby
load 'rubigraph-mine.rb'
load 'RubiGraphBuilder.rb'

class DynamicUbigraph

  attr_reader :rubiGraph
  
  def initialize
    Rubigraph.init
    Rubigraph.clear
    @rubiGraph = RubiGraphBuilder.new()
  end
  
  def createCommunities()
    # Populate communities
    file = File.open("output/communityMapping.dot", "r")
    while (line = file.gets)
      info = "#{line}".split(' ')
      @rubiGraph.addToCommunity(info[0], info[1])
    end
    file.close
  end
  
  def work()  
    # Populate Nodes and Edges
    file = File.open("output/addRemoveNodesAndEdges.dot", "r")
    while (line = file.gets)
      info = "#{line}".split(' ')
      
      # Check for added lines
      if info[0] == ">"
        @rubiGraph.addNode(info[1])
        @rubiGraph.addNode(info[2])
        @rubiGraph.addEdge(info[1], info[2])
      elsif info[0] == "<"
        @rubiGraph.removeEdge(info[1], info[2])
        @rubiGraph.removeNode(info[1])
        @rubiGraph.removeNode(info[2])
      end
    end
    file.close
    Rubigraph.flush!
  end
  
end
