#!/usr/bin/env ruby
require 'thread'

mutex1 = Mutex.new
mutex2 = Mutex.new
mutex3 = Mutex.new
threadCount = 8
threads = []

communities = {}
subgraphs = {}
previousNodes = Array.new
nonSubGraphNodes = Array.new
colors = ["#FF00FF", "#00FF00", "#CC00FF", "#33FF00", "#9900FF", "#66FF00", "#6600FF", "#99FF00", "#3300FF", "#CCFF00", "#0000FF", "#FFFF00", "#FF00CC", "#00FF33", "#FF33FF", "#33FF33", "#CC33FF", "#66FF33", "#9933FF", "#99FF33", "#6633FF", "#CCFF33", "#3333FF", "#FFFF33", "#0033FF", "#FFCC00", "#FF0099", "#00FF66", "#FF33CC", "#33FF66", "#FF66FF", "#66FF66", "#CC66FF", "#99FF66", "#9966FF", "#CCFF66", "#6666FF", "#FFFF66", "#3366FF", "#FFCC33", "#0066FF", "#FF9900", "#FF0066", "#00FF99", "#FF3399", "#33FF99", "#FF66CC", "#66FF99", "#FF99FF", "#99FF99", "#CC99FF", "#CCFF99", "#9999FF", "#FFFF99", "#6699FF", "#FFCC66", "#3399FF", "#FF9933", "#0099FF", "#FF6600", "#FF0000", "#00FFCC", "#FF3333", "#33FFCC", "#FF6666", "#66FFCC", "#FF9999", "#99FFCC", "#FFCCCC", "#CCFFCC", "#CCFFFF", "#FFFFCC", "#99FFFF", "#FFCC99", "#66FFFF", "#FF9966", "#33FFFF", "#FF6633", "#00FFFF", "#FF3300"]

File.open("output/communitySubGraphs.dot", "w") do |f|
  f.write("graph communities { \n  edge[dir=none, color=black]; \n  node[shape=point, color=red];\n  overlap=false;\n")
  
  # Read community data and an array for each community
  file = File.open("output/communityMapping.dot", "r")
  arr = file.readlines
  gap = arr.length / threadCount
  threadCount.times do |i|
    lines = arr.drop((i % threadCount) * gap)
    if i < threadCount - 1
      lines = lines.take(gap)
    end
    
    threads << Thread.new do
      lines.each do |line|
        info = "#{line}".split(' ')
      
        mutex1.synchronize do
          communities[info[0]] = info[1]
          
          if !subgraphs.has_key?(info[1])
            subgraphs[info[1]] = "\n  subgraph cluster_" + info[1] + " {\n    label = \"Cluster" + info[1] + "\";\n    edge[dir=none, color=\"" + colors[(info[1].to_i % colors.length)] + "\"];\n"
          end
        end
      end
    end
  end
  file.close
  threads.each(&:join)
  threads = []
  
  # Read original graph file and add to the community hash if they are in that community
  file = File.open("output/graph.dot", "r")
  arr = file.readlines
  gap = arr.length / threadCount
  threadCount.times do |i|
    lines = arr.drop((i % threadCount) * gap)
    if i < threadCount - 1
      lines = lines.take(gap)
    end
    
    threads << Thread.new do
      lines.each do |line|
        info = "#{line}".split(' ')
      
        if !info[0].nil? && !info[1].nil?
          if !(previousNodes.include?(info[0] + " -> " + info[1]) || previousNodes.include?(info[1] + " -> " + info[0]))
            if communities.has_key?(info[0]) && communities.has_key?(info[1]) && communities[info[0]] == communities[info[1]]
              mutex1.synchronize do
                subgraphs[communities[info[0]]] = subgraphs[communities[info[0]]] + "    " + info[0] + " -- " + info[1] + ";\n"
              end
            else
              mutex2.synchronize do 
                nonSubGraphNodes.push(info[0] + " -- " + info[1])
              end
            end

            mutex3.synchronize do
              previousNodes.push(info[0] + " -> " + info[1])
            end
          end
        end
      end
      
      Thread::exit()
    end
  end
  file.close
  threads.each(&:join)
  
  subgraphs.each do |key, value|
    f.write(value + "\n  }")
  end

  nonSubGraphNodes.each do |value|
    f.write("\n  " + value + ";")
  end
  
  f.write("\n}")
end
