#!/usr/bin/env ruby
require 'rubigraph'
Rubigraph.init

Rubigraph.clear

nodes = {}
edges = {}
edgeObjects = {}
communities = {}
communityColor = {}
colors = ["#FF00FF", "#CC00FF", "#9900FF", "#6600FF", "#3300FF", "#0000FF", "#FF00CC", "#FF33FF", "#CC33FF", "#9933FF", "#6633FF", "#3333FF", "#0033FF", "#FF0099", "#FF33CC", "#FF66FF", "#CC66FF", "#9966FF", "#6666FF", "#3366FF", "#0066FF", "#FF0066", "#FF3399", "#FF66CC", "#FF99FF", "#CC99FF", "#9999FF", "#6699FF", "#3399FF", "#0099FF", "#FF0000", "#FF3333", "#FF6666", "#FF9999", "#FFCCCC", "#FFFFFF", "#CCFFFF", "#99FFFF", "#66FFFF", "#33FFFF", "#00FFFF", "#FF3300", "#FF6633", "#FF9966", "#FFCC99", "#FFFFCC", "#CCFFCC", "#99FFCC", "#66FFCC", "#33FFCC", "#00FFCC", "#FF6600", "#FF9933", "#FFCC66", "#FFFF99", "#CCFF99", "#99FF99", "#66FF99", "#33FF99", "#00FF99", "#FF9900", "#FFCC33", "#FFFF66", "#CCFF66", "#99FF66", "#66FF66", "#33FF66", "#00FF66", "#FFCC00", "#FFFF33", "#CCFF33", "#99FF33", "#66FF33", "#33FF33", "#00FF33", "#FFFF00", "#CCFF00", "#99FF00", "#66FF00", "#33FF00", "#00FF00"]
counter = 0

# v1.remove
# e12.remove

# Read community data and build community map
file = File.open("output/communityMapping.dot", "r")
while (line = file.gets)
  info = "#{line}".split(' ')
  community = {info[0] => info[1]}
  communities.merge!(community)
  
  if communityColor[info[1]].nil?
    newColor = {info[1] => colors[counter]}
    communityColor.merge!(newColor)
    
    counter += 1
    if counter >= colors.length
      counter = 0
    end
  end
end
file.close

# Read original graph file and add all nodes and edges
file = File.open("output/graph.dot", "r")
while (line = file.gets)
  info = "#{line}".split(' ')
  
  if nodes[info[0]].nil?
    newNode = {info[0] => Rubigraph::Vertex.new}
    nodes.merge!(newNode)
    nodes[info[0]].shape = 'sphere'
    if !communities[info[0]].nil?
      nodes[info[0]].color = communityColor[communities[info[0]]]
    end
  end
  if nodes[info[1]].nil?
    newNode = {info[1] => Rubigraph::Vertex.new}
    nodes.merge!(newNode)
    nodes[info[1]].shape = 'sphere'
    if !communities[info[1]].nil?
      nodes[info[1]].color = communityColor[communities[info[1]]]
    end
  end
  
  if edges[info[1]].nil? || (!edges[info[1]].nil? && edges[info[1]] != info[0])
    newEdge = {info[0] => info[1]}
    edges.merge!(newEdge)
    
    newEdgeObject = {info[0] + " -> " + info[1] => Rubigraph::Edge.new(nodes[info[0]], nodes[info[1]])}
    edgeObjects.merge!(newEdgeObject)
  end
end
file.close
