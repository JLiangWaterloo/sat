#!/usr/bin/env ruby
load 'Helpers/rubigraph-mine.rb'

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
    node.color = color
    
    return node
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
