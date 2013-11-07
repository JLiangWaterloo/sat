#!/usr/bin/env ruby

class EdgePlotTools
  
  def clear()
    return true
  end
  
  def newNode(node, color)
    return node
  end
  
  def newEdge(node0, node1)
    return node0.to_s + " -> " + node1.to_s
  end
  
  def newCommunityEdge(edge, color, community)
    return true
  end
  
  def newIntercommunityEdge(edge)
    return true
  end
  
  def removeNode(node)
    return true
  end
  
  def removeEdge(edge)
    return true
  end
  
  def newCommunity(community, color)
    return true
  end
end
