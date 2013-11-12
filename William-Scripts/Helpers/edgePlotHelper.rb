#!/usr/bin/env ruby

class EdgePlotHelper

  def initialize(dir_name)
    @path = 'output/' + dir_name + '/'
    @graph = GraphBuilder.new("plot")
    system 'rm -f ' + @path + 'edgeTypeCountData.txt'
    system 'rm -f ' + @path + 'removedEdgeTypeCountData.txt'
    @i = 0
  end
  
  def work()
    puts '--- Pass ' + @i.to_s + ' ---'
    puts 'Applying Bcp, Graph, and Snap'
    
    system 'cat ' + @path + 'dump.dimacs | ../Haskell/Bcp | ../Haskell/Graph variable > ' + @path + 'graph' + @i.to_s + '.dot'
    system 'cat ' + @path + 'graph' + @i.to_s + '.dot | ../Bin/community -i:/dev/stdin -o:/dev/stdout | grep -v "#" > ' + @path + 'communityMapping.dot'
    
    if @i == 0
      system 'diff /dev/null ' + @path + 'graph' + @i.to_s + '.dot > ' + @path + 'addRemoveNodesAndEdges.dot'
    else
      system 'diff ' + @path + 'graph' + (@i - 1).to_s + '.dot ' + @path + 'graph' + @i.to_s + '.dot > ' + @path + 'addRemoveNodesAndEdges.dot'
      system 'rm -f ' + @path + 'graph' + (@i - 1).to_s + '.dot'
    end
    
    workOnDiff()
    populateDiffInformation()
    
    @graph.clear()
    createCommunities()
    createNodesAndEdges()
    @graph.color()
    populateCommunityInformation()
    @i += 1
  end
  
  def finish()
    puts "Finalizing"
    @i = 0
    system 'gnuplot -persist GnuplotScripts/CommunityVsIntercommunityTotal.gnu'
    system 'gnuplot -persist GnuplotScripts/CommunityVsIntercommunityRemoved.gnu'
  end
  
  def createCommunities()
    puts "Creating Communities"
    # Populate communities
    file = File.open(@path + "communityMapping.dot", "r")
    file.readlines.each do |line|
      info = "#{line}".split(' ')
      @graph.addToCommunity(info[0], info[1])
    end
    file.close
  end
  
  def createNodesAndEdges()
    puts "Adding Nodes and Edges"
    # Populate nodes and edges
    file = File.open(@path + "graph" + @i.to_s + ".dot", "r")
    file.readlines.each do |line|
      info = "#{line}".split(' ')
      @graph.addNode(info[0])
      @graph.addNode(info[1])
      @graph.addEdge(info[0], info[1])
    end
    file.close
  end
  
  def workOnDiff()
    puts "Adding and Removing Nodes and Edges"
    # Populate Nodes and Edges
    file = File.open(@path + "addRemoveNodesAndEdges.dot", "r")
    file.readlines.each do |line|
      info = "#{line}".split(' ')
      
      # Check for added lines
      if info[0] == "<"
        @graph.removeEdge(info[1], info[2])
      end
    end
    file.close
  end
  
  def populateDiffInformation()
    puts "Populating Node and Edge Information"
    system 'echo "' + @i.to_s + ' ' + @graph.getRemovedCommunityEdgeCount().to_s + ' ' + @graph.getRemovedIntercommunityEdgeCount().to_s + '" >> output/removedEdgeTypeCountData.txt'
    
  end
  
  def populateCommunityInformation()
    puts "Populating Community Information"
    system 'echo "' + @i.to_s + ' ' + @graph.getCommunityEdgeCount().to_s + ' ' + @graph.getIntercommunityEdgeCount().to_s + '" >> output/edgeTypeCountData.txt'
  end

end
