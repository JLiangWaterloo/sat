#!/usr/bin/env ruby
load 'Helpers/graphTools.rb'
load 'Helpers/edgePlotTools.rb'
load 'Helpers/ubigraphTools.rb'

class GraphBuilder
  
  COLORS = ["#FF00FF", "#00FF00", "#CC00FF", "#33FF00", "#9900FF", "#66FF00", "#6600FF", "#99FF00", "#3300FF", "#CCFF00", "#0000FF", "#FFFF00", "#FF00CC", "#00FF33", "#FF33FF", "#33FF33", "#CC33FF", "#66FF33", "#9933FF", "#99FF33", "#6633FF", "#CCFF33", "#3333FF", "#FFFF33", "#0033FF", "#FFCC00", "#FF0099", "#00FF66", "#FF33CC", "#33FF66", "#FF66FF", "#66FF66", "#CC66FF", "#99FF66", "#9966FF", "#CCFF66", "#6666FF", "#FFFF66", "#3366FF", "#FFCC33", "#0066FF", "#FF9900", "#FF0066", "#00FF99", "#FF3399", "#33FF99", "#FF66CC", "#66FF99", "#FF99FF", "#99FF99", "#CC99FF", "#CCFF99", "#9999FF", "#FFFF99", "#6699FF", "#FFCC66", "#3399FF", "#FF9933", "#0099FF", "#FF6600", "#FF0000", "#00FFCC", "#FF3333", "#33FFCC", "#FF6666", "#66FFCC", "#FF9999", "#99FFCC", "#FFCCCC", "#CCFFCC", "#CCFFFF", "#FFFFCC", "#99FFFF", "#FFCC99", "#66FFFF", "#FF9966", "#33FFFF", "#FF6633", "#00FFFF", "#FF3300"]
  
  #
  # Initializer
  #
  def initialize(type)
    if type == "graphviz"
      @tools = GraphTools.new()
    elsif type == "ubigraph"
      @tools = UbigraphTools.new()
    elsif type == "plot"
      @tools = EdgePlotTools.new()
    end

    clear()
  end
  
  #
  # Clear information
  #
  def clear
    @nodes = {}
    @edge_objects = {}
    @edge_counter = {}
    clearCommunities()    
  end
  
  #
  # Clear the communities
  #
  def clearCommunities()
    @tools.clear()
    @communities = {}
    @community_color = {}
    @communityEdgeCount = 0
    @intercommunityEdgeCount = 0
    @removedIntercommunityEdgeCount = 0
    @removedCommunityEdgeCount = 0
  end
  
  #
  # Adds the given node and community to the mapping with a color
  #
  def addToCommunity(node, community)
    @communities[node] = community
    
    if @community_color[community].nil?
      @tools.newCommunity(community, COLORS[(community.to_i % COLORS.length)])
      @community_color[community] = COLORS[(community.to_i % COLORS.length)]
    end
  end
  
  #
  # Adds the given node to the graph
  #
  def addNode(node)
    if @nodes[node].nil?
      @nodes[node] = @tools.newNode(node, @community_color[@communities[node]])
      @edge_counter[node] = 0
    end
  end
  
  #
  # Adds and edge between the two given nodes
  #
  def addEdge(node0, node1)
    edge = node0 + " -- " + node1
    if !node0.nil? && !node1.nil? && @edge_objects[edge].nil? && @edge_objects[node1 + " -- " + node0].nil?
      @edge_objects[edge] = @tools.newEdge(@nodes[node0], @nodes[node1])
      
      @edge_counter[node0] = (@edge_counter[node0] + 1)
      @edge_counter[node1] = (@edge_counter[node1] + 1)
    end
  end
  
  #
  # Color the Graph
  #
  def color()
    @nodes.each do |node|
      @tools.colorNode(node, @community_color[@communities[node]])
    end
    @edge_objects.each do |key, value|
      info = key.split(" -- ")
      
      if !@communities[info[0]].nil? && !@communities[info[1]].nil? && @communities[info[0]] == @communities[info[1]]
        @tools.newCommunityEdge(value, @community_color[@communities[info[0]]], @communities[info[0]])
        @communityEdgeCount += 1
      else
        @tools.newIntercommunityEdge(key)
        @intercommunityEdgeCount += 1
      end
    end
  end
  
  #
  # Removes the given node if there aren't any other edges going to it
  #
  def removeNode(node)
    if @edge_counter[node] <= 0 && !@nodes[node].nil?
      @tools.removeNode(@nodes[node])
      @nodes.delete(node)
    end
  end
  
  #
  # Removes the given edge
  #
  def removeEdge(node0, node1)
    edge = node0 + " -- " + node1
    if !@edge_objects[edge].nil?
      removeEdgeCount(node0, node1)
    
      @tools.removeEdge(@edge_objects[edge])
      @edge_objects.delete(edge)
  
      @edge_counter[node0] = (@edge_counter[node0] - 1)
      @edge_counter[node1] = (@edge_counter[node1] - 1)
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
  
  #
  # Get Subgraphs
  #
  def getSubgraphs
    return @tools.getSubgraphs()
  end
  
  #
  # Get NonSubgraphNodes
  #
  def getNonSubgraphNodes
    return @tools.getNonSubgraphNodes
  end
end
