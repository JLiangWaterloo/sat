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
    puts 'Applying Graph, and Snap'
    @time1 = Time.now
    
    system 'cat ' + @path + 'dump.dimacs | ../Haskell/Graph variable > ' + @path + 'graph' + @i.to_s + '.dot'
    system 'cat ' + @path + 'graph' + @i.to_s + '.dot | ../Bin/community -i:/dev/stdin -o:/dev/stdout | grep -v "#" > ' + @path + 'communityMapping.dot'
    
    if @i == 0
      system 'diff /dev/null ' + @path + 'graph' + @i.to_s + '.dot > ' + @path + 'addRemoveNodesAndEdges.dot'
    else
      system 'diff ' + @path + 'graph' + (@i - 1).to_s + '.dot ' + @path + 'graph' + @i.to_s + '.dot > ' + @path + 'addRemoveNodesAndEdges.dot'
      system 'rm -f ' + @path + 'graph' + (@i - 1).to_s + '.dot'
    end
    printTime()
    
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
    @time1 = Time.now
    
    # Populate communities
    file = File.open(@path + "communityMapping.dot", "r")
    file.readlines.each do |line|
      info = "#{line}".split(' ')
      @graph.addToCommunity(info[0], info[1])
    end
    file.close
    printTime()
  end
  
  def createNodesAndEdges()
    puts "Adding Nodes and Edges"
    @time1 = Time.now
    
    # Populate nodes and edges
    file = File.open(@path + "graph" + @i.to_s + ".dot", "r")
    file.readlines.each do |line|
      info = "#{line}".split(' ')
      @graph.addNode(info[0])
      @graph.addNode(info[1])
      @graph.addEdge(info[0], info[1])
    end
    file.close
    printTime()
  end
  
  def workOnDiff()
    puts "Adding and Removing Nodes and Edges"
    @time1 = Time.now
    
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
    printTime()
  end
  
  def populateDiffInformation()
    puts "Populating Node and Edge Information"
    @time1 = Time.now
    
    system 'echo "' + @i.to_s + ' ' + @graph.getRemovedCommunityEdgeCount().to_s + ' ' + @graph.getRemovedIntercommunityEdgeCount().to_s + '" >> output/removedEdgeTypeCountData.txt'
    printTime()
  end
  
  def populateCommunityInformation()
    puts "Populating Community Information"
    @time1 = Time.now
    
    system 'echo "' + @i.to_s + ' ' + @graph.getCommunityEdgeCount().to_s + ' ' + @graph.getIntercommunityEdgeCount().to_s + '" >> output/edgeTypeCountData.txt'
    printTime()
  end
  
  def printTime()
    time2 = Time.now
    puts "    Time = " + ((time2 - @time1)).to_s + "s"
    @time1 = time2
  end

end
