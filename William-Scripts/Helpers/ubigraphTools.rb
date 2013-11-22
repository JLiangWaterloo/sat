#!/usr/bin/env ruby
DIR_NAME_UBIGRAPH_TOOLS = File.expand_path File.dirname(__FILE__)

load DIR_NAME_UBIGRAPH_TOOLS + '/rubigraph-mine.rb'

class UbigraphTools

  def clear()
    return true
  end

  def initialize()
    Rubigraph.init
    Rubigraph.clear
  end
  
  def newNode(n, color)
    node = Rubigraph::Vertex.new
    node.shape = 'sphere'
    
    return node
  end
  
  def colorNode(node, color)
    node.color = color
  end
  
  def newEdge(node0, node1)
    edge = Rubigraph::Edge.new(node0, node1)
    return edge
  end
  
  def newCommunityEdge(edge, color, community)
    edge.color = color
  end
  
  def newIntercommunityEdge(edge)
    return true
  end
  
  def removeNode(node)
    node.remove
  end
  
  def removeEdge(edge)
    edge.remove
  end
  
  def newCommunity(community, color)
    return true
  end
end
