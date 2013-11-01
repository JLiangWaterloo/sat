#!/usr/bin/env ruby
load 'rubigraph-mine.rb'

class RubiPlotBuilder
  
  #
  # Member Variables and static variables
  #
  attr_reader :nodes
  attr_reader :edge_objects
  attr_reader :communities
  attr_reader :communityEdgeCount
  attr_reader :intercommunityEdgeCount
  attr_reader :removedCommunityEdgeCount
  attr_reader :removedIntercommunityEdgeCount
  
  #
  # Initializer
  #
  def initialize
    clear()
  end
  
  #
  # Clear information
  #
  def clear
    @nodes = {}
    @edge_objects = {}
    @communities = {}
    @communityEdgeCount = 0
    @intercommunityEdgeCount = 0
    @removedIntercommunityEdgeCount = 0
    @removedCommunityEdgeCount = 0
  end
  
  #
  # Adds the given node and community to the mapping
  #
  def addToCommunity(node, community)
    communities = {node => community}
    @communities.merge!(communities)
  end
  
  #
  # Adds the given node to the graph
  #
  def addNode(node)
    if @nodes[node].nil?
      newNode = {node => node}
      @nodes.merge!(newNode)
    end
  end
  
  #
  # Adds and edge between the two given nodes
  #
  def addEdge(node0, node1)
    if !node0.nil? && !node1.nil? && @edge_objects[node1 + " -> " + node0].nil? && @edge_objects[node0 + " -> " + node1].nil?
      newEdgeObject = {node0 + " -> " + node1 => node0 + " -> " + node1}
      @edge_objects.merge!(newEdgeObject)
      
      if !@communities[node0].nil? && !@communities[node1].nil? && @communities[node0] == @communities[node1]
        @communityEdgeCount += 1
      else
        @intercommunityEdgeCount += 1
      end
    end
  end
  
  #
  # Removes the given edge and checks both variations of it
  #
  def removeEdge(node0, node1)
    if !@edge_objects[node0 + " -> " + node1].nil?
      removeEdgeCount(node0, node1)
    
      @edge_objects.delete(node0 + " -> " + node1)
    end
  end
  
  #
  # Modify counters appropriately for removal of edge
  #
  def removeEdgeCount(node0, node1)
    if !@communities[node0].nil? && !@communities[node1].nil? && @communities[node0] == @communities[node1]
      @removedCommunityEdgeCount += 1
      @communityEdgeCount -= 1
    else
      @removedIntercommunityEdgeCount += 1
      @intercommunityEdgeCount -= 1
    end
  end
  
  #
  # Get removedCommunityEdgeCount
  #
  def getRemovedCommunityEdgeCount
    return @removedCommunityEdgeCount
  end
  
  #
  # Get removedIntercommunityEdgeCount
  #
  def getRemovedIntercommunityEdgeCount
    return @removedIntercommunityEdgeCount
  end
  
  #
  # Get communityEdgeCount
  #
  def getCommunityEdgeCount
    return @communityEdgeCount
  end
  
  #
  # Get intercommunityEdgeCount
  #
  def getIntercommunityEdgeCount
    return @intercommunityEdgeCount
  end
end
