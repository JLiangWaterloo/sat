#!/usr/bin/env ruby

class EdgePlotHelper

  def initialize()
    @graph = GraphBuilder.new("plot")
    system 'rm -f output/edgeTypeCountData.txt'
    system 'rm -f output/removedEdgeTypeCountData.txt'
    @i = 0
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
    
    workOnDiff()
    populateDiffInformation(@i)
    
    @graph.clear()
    createCommunities()
    createNodesAndEdges(@i)
    populateCommunityInformation(@i)
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
    file = File.open("output/communityMapping.dot", "r")
    while (line = file.gets)
      info = "#{line}".split(' ')
      @graph.addToCommunity(info[0], info[1])
    end
    file.close
  end
  
  def createNodesAndEdges(i)
    puts "Adding Nodes and Edges"
    # Populate nodes and edges
    file = File.open("output/graph" + i.to_s + ".dot", "r")
    while (line = file.gets)
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
    file = File.open("output/addRemoveNodesAndEdges.dot", "r")
    while (line = file.gets)
      info = "#{line}".split(' ')
      
      # Check for added lines
      if info[0] == "<"
        @graph.removeEdge(info[1], info[2])
      end
    end
    file.close
  end
  
  def populateDiffInformation(dumpCount)
    puts "Populating Node and Edge Information"
    system 'echo "' + dumpCount.to_s + ' ' + @graph.getRemovedCommunityEdgeCount().to_s + ' ' + @graph.getRemovedIntercommunityEdgeCount().to_s + '" >> output/removedEdgeTypeCountData.txt'
    
  end
  
  def populateCommunityInformation(dumpCount)
    puts "Populating Community Information"
    system 'echo "' + dumpCount.to_s + ' ' + @graph.getCommunityEdgeCount().to_s + ' ' + @graph.getIntercommunityEdgeCount().to_s + '" >> output/edgeTypeCountData.txt'
  end

end
