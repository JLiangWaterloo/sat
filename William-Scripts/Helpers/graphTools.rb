#!/usr/bin/env ruby

class GraphTools

  def initialize()
    clear()
  end
  
  def clear()
    @subgraphs = {}
    @nonSubGraphNodes = Array.new
  end
  
  def newNode(node, color)
    return node
  end
  
  def newEdge(node0, node1)
    return node0 + " -- " + node1
  end
  
  def newCommunityEdge(edge, color, community)
    @subgraphs[community] = @subgraphs[community] + "    " + edge + ";\n"
  end
  
  def newIntercommunityEdge(edge)
    @nonSubGraphNodes.push(edge)
  end
  
  def removeNode(node)
    return true
  end
  
  def removeEdge(edge)
    return true
  end
  
  def newCommunity(community, color)
    @subgraphs[community] = "\n  subgraph cluster_" + community + " {\n    label = \"Cluster" + community + "\";\n    edge[dir=none, color=\"" + color + "\"];\n"
  end
  
  def getSubgraphs()
    return @subgraphs
  end
  
  def getNonSubgraphNodes()
    return @nonSubGraphNodes
  end
end
