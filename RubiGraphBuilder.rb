#!/usr/bin/env ruby
load 'rubigraph-mine.rb'

class RubiGraphBuilder
  
  #
  # Member Variables and static variables
  #
  NODES = {}
  EDGE_OBJECTS = {}
  EDGE_COUNTER = {}
  COMMUNITIES = {}
  COMMUNITIES_ADDED = {}
  COMMUNITY_COLOR = {}
  @@counter = 0
  @@communityEdgeCount = 0
  @@intercommunityEdgeCount = 0
  @@removedIntercommunityEdgeCount = 0
  @@removedCommunityEdgeCount = 0
  
  COLORS = ["#FF00FF", "#00FF00", "#CC00FF", "#33FF00", "#9900FF", "#66FF00", "#6600FF", "#99FF00", "#3300FF", "#CCFF00", "#0000FF", "#FFFF00", "#FF00CC", "#00FF33", "#FF33FF", "#33FF33", "#CC33FF", "#66FF33", "#9933FF", "#99FF33", "#6633FF", "#CCFF33", "#3333FF", "#FFFF33", "#0033FF", "#FFCC00", "#FF0099", "#00FF66", "#FF33CC", "#33FF66", "#FF66FF", "#66FF66", "#CC66FF", "#99FF66", "#9966FF", "#CCFF66", "#6666FF", "#FFFF66", "#3366FF", "#FFCC33", "#0066FF", "#FF9900", "#FF0066", "#00FF99", "#FF3399", "#33FF99", "#FF66CC", "#66FF99", "#FF99FF", "#99FF99", "#CC99FF", "#CCFF99", "#9999FF", "#FFFF99", "#6699FF", "#FFCC66", "#3399FF", "#FF9933", "#0099FF", "#FF6600", "#FF0000", "#00FFCC", "#FF3333", "#33FFCC", "#FF6666", "#66FFCC", "#FF9999", "#99FFCC", "#FFCCCC", "#CCFFCC", "#CCFFFF", "#FFFFCC", "#99FFFF", "#FFCC99", "#66FFFF", "#FF9966", "#33FFFF", "#FF6633", "#00FFFF", "#FF3300", "#FFFFFF"]

  #
  # Builds entire graph without modifications from communityMapping.dot and graph.dot
  #
  def buildOriginalGraph
    # Read community data and build community map
    file = File.open("output/communityMapping.dot", "r")
    while (line = file.gets)
      info = "#{line}".split(' ')
      addToOriginalCommunity(info[0], info[1])
    end
    file.close
    
    # Read original graph file and add all NODES and edges
    file = File.open("output/graph.dot", "r")
    while (line = file.gets)
      info = "#{line}".split(' ')
      
      addNode(info[0])
      addNode(info[1])
      addEdge(info[0], info[1])
    end
    file.close
  end
  
  #
  # Adds the given node and community to the mapping with a color
  #
  def addToOriginalCommunity(node, community)
    communities = {node => community}
    COMMUNITIES.merge!(communities)
    
    if COMMUNITY_COLOR[community].nil?
      newColor = {community => COLORS[@@counter]}
      COMMUNITY_COLOR.merge!(newColor)
      
      @@counter += 1
      if @@counter >= COLORS.length
        @@counter = 0
      end
    end
  end
  
  #
  # Adds the given node and community to the added mapping 
  #
  def addToAddedCommunity(node, community)
    communities = {node => community}
    COMMUNITIES_ADDED.merge!(communities)
  end
  
  #
  # Removes the given node from the added mapping 
  #
  def removeFromAddedCommunity(node)
    COMMUNITIES_ADDED.delete(node)
  end
  
  #
  # Adds the given node to the graph
  #
  def addNode(node)
    if NODES[node].nil?
      newNode = {node => Rubigraph::Vertex.new}
      NODES.merge!(newNode)
      NODES[node].shape = 'sphere'
      if !COMMUNITIES[node].nil?
        NODES[node].color = COMMUNITY_COLOR[COMMUNITIES[node]]
      end
      
      newCounter = { node => 0}
      EDGE_COUNTER.merge!(newCounter)
    end
  end
  
  #
  # Adds and edge between the two given nodes
  #
  def addEdge(node0, node1)
    if !node0.nil? && !node1.nil? && EDGE_OBJECTS[node1 + " -> " + node0].nil? && EDGE_OBJECTS[node0 + " -> " + node1].nil?
      newEdgeObject = {node0 + " -> " + node1 => Rubigraph::Edge.new(NODES[node0], NODES[node1])}
      EDGE_OBJECTS.merge!(newEdgeObject)
      
      if COMMUNITIES[node0] == COMMUNITIES[node1]
        EDGE_OBJECTS[node0 + " -> " + node1].color = COMMUNITY_COLOR[COMMUNITIES[node0]]
        @@communityEdgeCount += 1
      else
        @@intercommunityEdgeCount += 1
      end
      
      newCounter = { node0 => (EDGE_COUNTER[node0] + 1), node1 => (EDGE_COUNTER[node1] + 1)}
      EDGE_COUNTER.merge!(newCounter)
    end
  end
  
  #
  # Removes the given node if there aren't any other edges going to it
  #
  def removeNode(node)
    if EDGE_COUNTER[node] <= 0 && !NODES[node].nil?
      NODES[node].remove
      NODES.delete(node)
    end
  end
  
  #
  # Removes the given edge and checks both variations of it
  #
  def removeEdge(node0, node1)
    if !EDGE_OBJECTS[node0 + " -> " + node1].nil?
      removeEdgeCount(node0, node1)
    
      EDGE_OBJECTS[node0 + " -> " + node1].remove
      EDGE_OBJECTS.delete(node0 + " -> " + node1)
  
      newCounter = {node0 => (EDGE_COUNTER[node0] - 1), node1 => (EDGE_COUNTER[node1] - 1)}
      EDGE_COUNTER.merge!(newCounter)
    end
  end
  
  #
  # Modify counters appropriately for removal of edge
  #
  def removeEdgeCount(node0, node1)
    if !COMMUNITIES_ADDED[node0].nil? && !COMMUNITIES_ADDED[node1].nil? && COMMUNITIES_ADDED[node0] == COMMUNITIES_ADDED[node1]
      @@removedCommunityEdgeCount += 1
      @@communityEdgeCount -= 1
    else
      @@removedIntercommunityEdgeCount += 1
      @@intercommunityEdgeCount -= 1
    end
  end
  
  #
  # Get removedCommunityEdgeCount
  #
  def getRemovedCommunityEdgeCount
    return @@removedCommunityEdgeCount
  end
  
  #
  # Get removedIntercommunityEdgeCount
  #
  def getRemovedIntercommunityEdgeCount
    return @@removedIntercommunityEdgeCount
  end
  
  #
  # Get communityEdgeCount
  #
  def getCommunityEdgeCount
    return @@communityEdgeCount
  end
  
  #
  # Get intercommunityEdgeCount
  #
  def getIntercommunityEdgeCount
    return @@intercommunityEdgeCount
  end
  
  #
  # Clear removedCommunityEdgeCount
  #
  def clearRemovedCommunityEdgeCount
    @@removedCommunityEdgeCount = 0
  end
  
  #
  # Clear removedIntercommunityEdgeCount
  #
  def clearRemovedIntercommunityEdgeCount
    @@removedIntercommunityEdgeCount = 0
  end
end
