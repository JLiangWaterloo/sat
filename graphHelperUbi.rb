#!/usr/bin/env ruby
require 'rubigraph'
Rubigraph.init

Rubigraph.clear

nodes = {}
edgeObjects = {}
communities = {}
communityColor = {}
colors = ["#FF00FF", "#00FF00", "#CC00FF", "#33FF00", "#9900FF", "#66FF00", "#6600FF", "#99FF00", "#3300FF", "#CCFF00", "#0000FF", "#FFFF00", "#FF00CC", "#00FF33", "#FF33FF", "#33FF33", "#CC33FF", "#66FF33", "#9933FF", "#99FF33", "#6633FF", "#CCFF33", "#3333FF", "#FFFF33", "#0033FF", "#FFCC00", "#FF0099", "#00FF66", "#FF33CC", "#33FF66", "#FF66FF", "#66FF66", "#CC66FF", "#99FF66", "#9966FF", "#CCFF66", "#6666FF", "#FFFF66", "#3366FF", "#FFCC33", "#0066FF", "#FF9900", "#FF0066", "#00FF99", "#FF3399", "#33FF99", "#FF66CC", "#66FF99", "#FF99FF", "#99FF99", "#CC99FF", "#CCFF99", "#9999FF", "#FFFF99", "#6699FF", "#FFCC66", "#3399FF", "#FF9933", "#0099FF", "#FF6600", "#FF0000", "#00FFCC", "#FF3333", "#33FFCC", "#FF6666", "#66FFCC", "#FF9999", "#99FFCC", "#FFCCCC", "#CCFFCC", "#CCFFFF", "#FFFFCC", "#99FFFF", "#FFCC99", "#66FFFF", "#FF9966", "#33FFFF", "#FF6633", "#00FFFF", "#FF3300", "#FFFFFF"]
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
  
  if !info[0].nil? && !info[1].nil? && edgeObjects[info[1] + " -> " + info[0]].nil? && edgeObjects[info[0] + " -> " + info[1]].nil?
    newEdgeObject = {info[0] + " -> " + info[1] => Rubigraph::Edge.new(nodes[info[0]], nodes[info[1]])}
    edgeObjects.merge!(newEdgeObject)
    
    if communities[info[0]] == communities[info[1]]
      edgeObjects[info[0] + " -> " + info[1]].color = communityColor[communities[info[0]]]
    end
  end
end
file.close
