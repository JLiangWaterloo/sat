#!/usr/bin/env ruby

class GraphvizHelper

  GIF = "gif"

  def initialize(file_name, file_type, details)
    @graph = GraphBuilder.new("graphviz")
    @i = 0
    @details = details
    @file_type = file_type
    @file_name = file_name
    
    if file_type == "gif"
      @ext = "jpg"
      @dir_name = File.basename( file_name, ".*" )
      system 'rm -rf EvolutionData/' + @dir_name
      system 'mkdir -p EvolutionData/' + @dir_name
    else
      @ext = "pdf"
    end
  end
  
  def work()
    system 'echo "-- Pass ' + @i.to_s + ' --"'
    system 'cat output/dump.dimacs | ../Haskell/Bcp | ../Haskell/Graph variable > output/graph' + @i.to_s + '.dot'
    system 'cat output/graph' + @i.to_s + '.dot | ../Bin/community -i:/dev/stdin -o:/dev/stdout | grep -v "#" > output/communityMapping.dot'
    
    if @i == 0
      system 'diff /dev/null output/graph' + @i.to_s + '.dot > output/addRemoveNodesAndEdges.dot'
    else
      system 'diff output/graph' + (@i - 1).to_s + '.dot output/graph' + @i.to_s + '.dot > output/addRemoveNodesAndEdges.dot'
      system 'rm -f output/graph' + (@i - 1).to_s + '.dot'
    end
    
    createCommunities()
    addRemoveNodesAndEdges()
    buildOutput()
    @i += 1
    
    if @details == "Y"
      type = 'dot'
    else
      type = 'sfdp'
    end
    system type + ' -T' + @ext + ' output/communitySubGraphs.dot -o output/communityGraph.' + @ext
    
    if @file_type == "gif"
      modularity = `cat #{@file_name} | ./CommunityOutputOnlyModularity`
      system 'convert output/communityGraph.' + @ext + ' -gravity north -stroke none -fill black -annotate 0 "Modularity = ' + modularity.to_s + '" output/communityGraph.' + @ext
      
      c = format('%04d', @i)
      system 'mv output/communityGraph.jpg EvolutionData/' + @dir_name
      system 'mv EvolutionData/' + @dir_name + '/communityGraph.jpg EvolutionData/' + @dir_name + '/' + c.to_s + '.jpg'
    end
  end
  
  def createCommunities()
    @graph.clearCommunities()
  
    # Populate communities
    file = File.open("output/communityMapping.dot", "r")
    file.readlines.each do |line|
      info = "#{line}".split(' ')
      @graph.addToCommunity(info[0], info[1])
    end
    file.close
  end
  
  def addRemoveNodesAndEdges()  
    # Populate Nodes and Edges
    file = File.open("output/addRemoveNodesAndEdges.dot", "r")
    file.readlines.each do |line|
      info = "#{line}".split(' ')
      
      # Check for added lines
      if info[0] == ">"
        @graph.addNode(info[1])
        @graph.addNode(info[2])
        @graph.addEdge(info[1], info[2])
      elsif info[0] == "<"
        Rubigraph.setPoolSize(1)
        @graph.removeEdge(info[1], info[2])
        @graph.removeNode(info[1])
        @graph.removeNode(info[2])
      end
    end
    file.close
  end
  
  def buildOutput()
    File.open("output/communitySubGraphs.dot", "w") do |f|
      f.write("graph communities { \n  edge[dir=none, color=black]; \n  node[shape=point, color=red];\n  overlap=false;\n")
      
      @graph.getSubgraphs.each do |key, value|
        f.write(value + "\n  }")
      end
      
      @graph.getNonSubgraphNodes.each do |value|
        f.write("\n  " + value + ";")
      end
      
      f.write("\n}")
    end
  end
  
  def finish()
    @i = 0
    
    if @file_type == "gif"
      buildGif()
    else
      system 'xdg-open output/communityGraph.' + @ext
    end
  end
  
  def buildGif()
    biggestWidth = 0
    size = "500x500"
    
    Dir['EvolutionData/' + @dir_name + '/*.jpg'].each do |item|
      filename = item.to_s
      width = `identify -format "%w" #{filename}`
      if width.to_i > biggestWidth
        size = `identify -format "%w"x"%h" #{filename}`
        biggestWidth = width.to_i
      end
    end

    system './Helpers/imagemagickHelper ' + @dir_name + ' ' + size
  end

end
