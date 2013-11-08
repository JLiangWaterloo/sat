#!/usr/bin/env ruby
require 'diffy'

class GraphvizHelper

  GIF = "gif"

  def initialize(file_name, file_type, details)
    @graph = GraphBuilder.new("graphviz")
    @i = 0
    @details = details
    @file_type = file_type
    @file_name = file_name
    
    @communityFile = ""
    @previousGraphFile = ""
    @graphFile = ""
    
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
    puts '--- Pass ' + @i.to_s + ' ---'
    puts 'Applying Bcp, Graph, and Snap'
    
    @graphFile = `cat output/dump.dimacs | ../Haskell/Bcp | ../Haskell/Graph variable`
    file = File.open("output/graph.dot", "w")
    file.write(@graphFile)
    file.close
    @communityFile = `cat output/graph.dot | ../Bin/community -i:/dev/stdin -o:/dev/stdout | grep -v "#"`
    @graphDiff = Diffy::Diff.new(@previousGraphFile, @graphFile).to_s
    @previousGraphFile = @graphFile
    
    createCommunities()
    addRemoveNodesAndEdges()
    buildOutput()
    @i += 1
    
    if @details == "Y"
      type = 'dot'
    else
      type = 'sfdp'
    end
    
    if @file_type == "gif"
      c = format('%04d', @i)
      system type + ' -T' + @ext + ' output/communitySubGraphs.dot -o EvolutionData/' + @dir_name + '/' + c.to_s + '.' + @ext
    
      modularity = `cat #{@file_name} | ./CommunityOutputOnlyModularity`
      system 'convert EvolutionData/' + @dir_name + '/' + c.to_s + '.' + @ext + ' -gravity north -stroke none -fill black -annotate 0 "Modularity = ' + modularity.to_s + '" EvolutionData/' + @dir_name + '/' + c.to_s + '.' + @ext
    else
      system type + ' -T' + @ext + ' output/communitySubGraphs.dot -o output/communityGraph.' + @ext
    end
  end
  
  def createCommunities()
    puts "Creating Communities"
    @graph.clearCommunities()
  
    # Populate communities
    @communityFile.each_line do |line|
      info = "#{line}".split(' ')
      @graph.addToCommunity(info[0], info[1])
    end
  end
  
  def addRemoveNodesAndEdges()
    puts "Adding and Removing Nodes and Edges"
    # Populate Nodes and Edges
    @graphDiff.each_line do |line|
      line =  line.gsub(/[+-]/, '+' => '+ ', '-' => '- ')
      info = "#{line}".split(' ')
      
      # Check for added lines
      if info[0] == "+"
        @graph.addNode(info[1])
        @graph.addNode(info[2])
        @graph.addEdge(info[1], info[2])
      elsif info[0] == "-"
        @graph.removeEdge(info[1], info[2])
        @graph.removeNode(info[1])
        @graph.removeNode(info[2])
      end
    end
  end
  
  def buildOutput()
    puts "Building image"
    File.open("output/communitySubGraphs.dot", "w") do |f|
      f.write("graph communities { \n  edge[dir=none, color=black]; \n  node[shape=point, color=red];\n  overlap=false;\n")
      
      @graph.getSubgraphs.each do |key, value|
        f.write(value + "\n  }")
      end
      f.write(@graph.getNonSubgraphNodes)
      
      f.write("\n}")
    end
  end
  
  def finish()
    puts "Finalizing"
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
