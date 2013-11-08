#!/usr/bin/env ruby

class EdgePlotHelper

  def initialize()
    @graph = GraphBuilder.new("plot")
    system 'rm -f output/edgeTypeCountData.txt'
    system 'rm -f output/removedEdgeTypeCountData.txt'
    @i = 0
    @communityFile = ""
    @previousGraphFile = ""
    @graphFile = ""
  end
  
  def work()
    puts '--- Pass ' + @i.to_s + ' ---'
    puts 'Applying Bcp, Graph, and Snap'
        
    @graphFile = `cat output/dump.dimacs | ../Haskell/Bcp | ../Haskell/Graph variable`
    @graphDiff = Diffy::Diff.new(@previousGraphFile, @graphFile).to_s
    @previousGraphFile = @graphFile
    
    file = File.open("output/graph.dot", "w")
    file.write(@graphFile)
    file.close
    @communityFile = `cat output/graph.dot | ../Bin/community -i:/dev/stdin -o:/dev/stdout | grep -v "#"`
    
    workOnDiff()
    populateDiffInformation()
    
    @graph.clear()
    createCommunities()
    createNodesAndEdges()
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
    @communityFile.each_line do |line|
      info = "#{line}".split(' ')
      @graph.addToCommunity(info[0], info[1])
    end
  end
  
  def createNodesAndEdges()
    puts "Adding Nodes and Edges"
    # Populate nodes and edges
    @graphFile.each_line do |line|
      info = "#{line}".split(' ')
      @graph.addNode(info[0])
      @graph.addNode(info[1])
      @graph.addEdge(info[0], info[1])
    end
  end
  
  def workOnDiff()
    puts "Adding and Removing Nodes and Edges"
    # Populate Nodes and Edges
    @graphDiff.each_line do |line|
      line =  line.gsub(/[+-]/, '+' => '+ ', '-' => '- ')
      info = "#{line}".split(' ')
      
      # Check for added lines
      if info[0] == "-"
        @graph.removeEdge(info[1], info[2])
      end
    end
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
