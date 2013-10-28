#!/usr/bin/env ruby
require 'rubigraph'

class RubiGraphBuilder
  
  #
  # Member Variables and static variables
  #
  @@nodes = {}
  @@edge_objects = {}
  @@edge_counter = {}
  @@communities = {}
  @@community_color = {}
  @@counter = 0
  COLORS = ["#FF00FF", "#00FF00", "#CC00FF", "#33FF00", "#9900FF", "#66FF00", "#6600FF", "#99FF00", "#3300FF", "#CCFF00", "#0000FF", "#FFFF00", "#FF00CC", "#00FF33", "#FF33FF", "#33FF33", "#CC33FF", "#66FF33", "#9933FF", "#99FF33", "#6633FF", "#CCFF33", "#3333FF", "#FFFF33", "#0033FF", "#FFCC00", "#FF0099", "#00FF66", "#FF33CC", "#33FF66", "#FF66FF", "#66FF66", "#CC66FF", "#99FF66", "#9966FF", "#CCFF66", "#6666FF", "#FFFF66", "#3366FF", "#FFCC33", "#0066FF", "#FF9900", "#FF0066", "#00FF99", "#FF3399", "#33FF99", "#FF66CC", "#66FF99", "#FF99FF", "#99FF99", "#CC99FF", "#CCFF99", "#9999FF", "#FFFF99", "#6699FF", "#FFCC66", "#3399FF", "#FF9933", "#0099FF", "#FF6600", "#FF0000", "#00FFCC", "#FF3333", "#33FFCC", "#FF6666", "#66FFCC", "#FF9999", "#99FFCC", "#FFCCCC", "#CCFFCC", "#CCFFFF", "#FFFFCC", "#99FFFF", "#FFCC99", "#66FFFF", "#FF9966", "#33FFFF", "#FF6633", "#00FFFF", "#FF3300", "#FFFFFF"]

  #
  # Builds entire graph without modifications from communityMapping.dot and graph.dot
  #
  def buildOriginalGraph
    # Read community data and build community map
    file = File.open("output/communityMapping.dot", "r")
    while (line = file.gets)
      info = "#{line}".split(' ')
      addToCommunity(info[0], info[1])
    end
    file.close
    
    # Read original graph file and add all @@nodes and edges
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
  def addToCommunity(node, community)
    communities = {node => community}
    @@communities.merge!(communities)
    
    if @@community_color[community].nil?
      newColor = {community => COLORS[@@counter]}
      @@community_color.merge!(newColor)
      
      @@counter += 1
      if @@counter >= COLORS.length
        @@counter = 0
      end
    end
  end
  
  #
  # Adds the given node to the graph
  #
  def addNode(node)
    if @@nodes[node].nil?
      newNode = {node => Rubigraph::Vertex.new}
      @@nodes.merge!(newNode)
      @@nodes[node].shape = 'sphere'
      if !@@communities[node].nil?
        @@nodes[node].color = @@community_color[@@communities[node]]
      end
      
      newCounter = { node => 0}
      @@edge_counter.merge!(newCounter)
    end
  end
  
  #
  # Adds and edge between the two given nodes
  #
  def addEdge(node0, node1)
    if !node0.nil? && !node1.nil? && @@edge_objects[node1 + " -> " + node0].nil? && @@edge_objects[node0 + " -> " + node1].nil?
      newEdgeObject = {node0 + " -> " + node1 => Rubigraph::Edge.new(@@nodes[node0], @@nodes[node1])}
      @@edge_objects.merge!(newEdgeObject)
      
      if @@communities[node0] == @@communities[node1]
        @@edge_objects[node0 + " -> " + node1].color = @@community_color[@@communities[node0]]
      end
      
      newCounter = { node0 => (@@edge_counter[node0] + 1), node1 => (@@edge_counter[node1] + 1)}
      @@edge_counter.merge!(newCounter)
    end
  end
  
  #
  # Removes the given node if there aren't any other edges going to it
  #
  def removeNode(node)
    if @@edge_counter[node] <= 0 && !@@nodes[node].nil?
      @@nodes[node].remove
      @@nodes.delete(node)
    end
  end
  
  #
  # Removes the given edge and checks both variations of it
  #
  def removeEdge(node0, node1)
    if !@@edge_objects[node0 + " -> " + node1].nil?
      @@edge_objects[node0 + " -> " + node1].remove
      @@edge_objects.delete(node0 + " -> " + node1)
      
      newCounter = {node0 => (@@edge_counter[node0] - 1), node1 => (@@edge_counter[node1] - 1)}
      @@edge_counter.merge!(newCounter)
    end
  end
end
