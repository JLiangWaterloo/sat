#!/usr/bin/env ruby

class UbigraphHelper

  def initialize()
    @graph = GraphBuilder.new("ubigraph")
    @i = 0
  end
  
  def work()
    system 'echo "-- Pass ' + @i.to_s + ' --"'
    system 'cat output/dump.dimacs | ../Haskell/Bcp | ../Haskell/Graph variable > output/graph' + @i.to_s + '.dot'
    
    if @i == 0
      system 'diff /dev/null output/graph' + @i.to_s + '.dot > output/addRemoveNodesAndEdges.dot'
      system 'cat output/graph' + @i.to_s + '.dot | ../Bin/community -i:/dev/stdin -o:/dev/stdout | grep -v "#" > output/communityMapping.dot'
      createCommunities()
    else
      system 'diff output/graph' + (@i - 1).to_s + '.dot output/graph' + @i.to_s + '.dot > output/addRemoveNodesAndEdges.dot'
      
      system 'rm -f output/graph' + (@i - 1).to_s + '.dot'
      
      system 'echo "Wait for 5 seconds"'
      system 'sleep 5'
    end
    
    addRemoveNodesAndEdges()
    @i += 1
  end
  
  def createCommunities()
    puts "Creating Communities"
    # Populate communities
    file = File.open("output/communityMapping.dot", "r")
    file.readlines.each do |line|
      info = "#{line}".split(' ')
      @graph.addToCommunity(info[0], info[1])
    end
    file.close
  end
  
  def addRemoveNodesAndEdges() 
    puts "Adding and Removing Nodes and Edges" 
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
#        Rubigraph.setPoolSize(1)
        @graph.removeEdge(info[1], info[2])
        @graph.removeNode(info[1])
        @graph.removeNode(info[2])
      end
    end
    file.close
  end
  
  def finish()
    puts "Finalizing"
    @i = 0
    return true
  end

end
