#!/usr/bin/env ruby
require 'thread'
DIR_NAME_GRAPHVIZ = File.expand_path File.dirname(__FILE__)

class GraphvizHelper

  GIF = "gif"

  def initialize(dir_name, type, details, output_format)
    @graph = GraphBuilder.new("graphviz")
    @i = 0
    @details = details
    @type = type
    @dir_name = dir_name
    @path = DIR_NAME_GRAPHVIZ + '/../output/' + dir_name + '/'
    @threads = []
    @ext = output_format
    
    if type == "evolution"
      system 'rm -rf ' + DIR_NAME_GRAPHVIZ + '/../EvolutionData/' + @dir_name
      system 'mkdir -p ' + DIR_NAME_GRAPHVIZ + '/../EvolutionData/' + @dir_name
    end
  end
  
  def work()
    puts '  Applying Graph, Snap and Diff'
    @time1 = Time.now
    
    if @type == "evolution"
      system 'cat ' + @path + 'dump.dimacs | ' + DIR_NAME_GRAPHVIZ + '/../../Haskell/Graph variable > ' + @path + 'graph' + @i.to_s + '.dot'
    else
      system 'cat ' + @path + 'dump.dimacs | ' + DIR_NAME_GRAPHVIZ + '/../../Haskell/Bcp | ' + DIR_NAME_GRAPHVIZ + '/../../Haskell/Graph variable > ' + @path + 'graph' + @i.to_s + '.dot'
    end
    system 'cat ' + @path + 'graph' + @i.to_s + '.dot | ' + DIR_NAME_GRAPHVIZ + '/../../Bin/community -i:/dev/stdin -o:/dev/stdout | grep -v "#" > ' + @path + 'communityMapping.dot'
    
    
    if @i == 0
      system 'diff /dev/null ' + @path + 'graph' + @i.to_s + '.dot > ' + @path + 'addRemoveNodesAndEdges.dot'
    else
      system 'diff ' + @path + 'graph' + (@i - 1).to_s + '.dot ' + @path + 'graph' + @i.to_s + '.dot > ' + @path + 'addRemoveNodesAndEdges.dot'
#      system 'rm -f ' + @path + 'graph' + (@i - 1).to_s + '.dot'
    end
    printTime()
    
    createCommunities()
    addRemoveNodesAndEdges()
    color()
    buildImageContent()
    buildImage()
    
    @i += 1
  end
  
  def createCommunities()
    puts "  Creating Communities"
    @time1 = Time.now
    @graph.clearCommunities()
  
    # Populate communities
    file = File.open(@path + "communityMapping.dot", "r")
    file.readlines.each do |line|
      info = "#{line}".split(' ')
      @graph.addToCommunity(info[0], info[1])
    end
    file.close
    printTime()
  end
  
  def addRemoveNodesAndEdges()
    puts "  Adding and Removing Nodes and Edges"
    @time1 = Time.now
    
    # Populate Nodes and Edges
    file = File.open(@path + "addRemoveNodesAndEdges.dot", "r")
    file.readlines.each do |line|
      info = "#{line}".split(' ')
      
      # Check for added lines
      if info[0] == ">"
        @graph.addNode(info[1])
        @graph.addNode(info[2])
        @graph.addEdge(info[1], info[2])
      elsif info[0] == "<"
        @graph.removeEdge(info[1], info[2])
        @graph.removeNode(info[1])
        @graph.removeNode(info[2])
      end
    end
    file.close
    printTime()
  end
  
  def color()
    puts "  Coloring Graph"
    @time1 = Time.now
    @graph.color()
    printTime()
  end
  
  def buildImageContent()
    puts "  Building image content"
    @time1 = Time.now
    
    File.open(@path + "communitySubGraphs_" + @i.to_s + ".dot", "w") do |f|
      f.write("graph communities { \n  edge[dir=none, color=black]; \n  node[shape=point, color=red];\n  overlap=false;\n")
      
      @graph.getSubgraphs.each do |key, value|
        f.write(value + "\n  }")
      end
      f.write(@graph.getNonSubgraphNodes)
      
      f.write("\n}")
    end
    printTime()
  end
  
  def buildImage()
    puts "  Building image on seperate thread"
    system 'cp ' + @path + 'dump.dimacs ' + @path + 'dump' + @i.to_s + '.dimacs'
    tmp = @i
    @threads << Thread.new do
      if @details == "Y"
        type = 'dot'
      else
        type = 'sfdp'
      end
      
      if @type == "evolution"
        c = format('%04d', tmp)
        output_path = DIR_NAME_GRAPHVIZ + '/../EvolutionData/' + @dir_name + '/' + c.to_s + '.' + @ext
      else
        output_path = @path + 'communityGraph.' + @ext
      end
      
      system type + ' -T' + @ext + ' ' + @path + 'communitySubGraphs_' + tmp.to_s + '.dot -o ' + output_path
      
      if @ext != "svg"
        modularity = `cat #{@path}dump#{tmp.to_s}.dimacs | #{DIR_NAME_GRAPHVIZ}/../CommunityOutputOnlyModularity`
        system 'convert ' + output_path + ' -gravity north -stroke none -fill black -annotate 0 "Modularity = ' + modularity.to_s + '" ' + output_path
      end
      
      Thread::exit()
    end
  end
  
  def finish()
    puts "Finalizing - Waiting for threads to terminate first"
    @threads.each(&:join)
    
    if @i < 30
      if @type == "evolution"
        buildGif()
      else
        system 'xdg-open ' + @path + 'communityGraph.' + @ext
      end
    else
      puts "Go to " + DIR_NAME_GRAPHVIZ + "/../EvolutionData/" + @dir_name + " to see the outputted images."
    end
    
    @i = 0
  end
  
  def buildGif()
    biggestWidth = 0
    size = "500x500"
    
    Dir[DIR_NAME_GRAPHVIZ + '/../EvolutionData/' + @dir_name + '/*.jpg'].each do |item|
      filename = item.to_s
      width = `identify -format "%w" #{filename}`
      if width.to_i > biggestWidth
        size = `identify -format "%w"x"%h" #{filename}`
        biggestWidth = width.to_i
      end
    end

    system DIR_NAME_GRAPHVIZ + '/imagemagickHelper ' + @dir_name + ' ' + size
  end
  
  def printTime()
    time2 = Time.now
    puts "    Time = " + ((time2 - @time1)).to_s + "s"
  end

end
