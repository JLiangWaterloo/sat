#!/usr/bin/env ruby
load 'RubiPlotBuilder.rb'

class PlotHelper

  attr_reader :plot
  
  def initialize
    @plot = RubiPlotBuilder.new()
  end
  
  def clear
    @plot.clear()
  end

  def createCommunities()
    # Populate communities
    file = File.open("output/communityMapping.dot", "r")
    while (line = file.gets)
      info = "#{line}".split(' ')
      @plot.addToCommunity(info[0], info[1])
    end
    file.close
  end
  
  def createNodesAndEdges(i)
    # Populate nodes and edges
    file = File.open("output/graph" + i.to_s + ".dot", "r")
    while (line = file.gets)
      info = "#{line}".split(' ')
      @plot.addNode(info[0])
      @plot.addNode(info[1])
      @plot.addEdge(info[0], info[1])
    end
    file.close
  end
  
  def workOnDiff()
    # Populate Nodes and Edges
    file = File.open("output/addRemoveNodesAndEdges.dot", "r")
    while (line = file.gets)
      info = "#{line}".split(' ')
      
      # Check for added lines
      if info[0] == "<"
        @plot.removeEdge(info[1], info[2])
      end
    end
    file.close
  end
  
  def populateDiffInformation(dumpCount)
    system 'echo "' + dumpCount.to_s + ' ' + @plot.getRemovedCommunityEdgeCount().to_s + ' ' + @plot.getRemovedIntercommunityEdgeCount().to_s + '" >> output/removedEdgeTypeCountData.txt'
    
  end
  
  def populateCommunityInformation(dumpCount)
    system 'echo "' + dumpCount.to_s + ' ' + @plot.getCommunityEdgeCount().to_s + ' ' + @plot.getIntercommunityEdgeCount().to_s + '" >> output/edgeTypeCountData.txt'
  end
end
