#!/usr/bin/env ruby

class UbigraphHelper

  def initialize()
    @graph = GraphBuilder.new("ubigraph")
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
    
    if @i == 0
      file = File.open("output/graph.dot", "w")
      file.write(@graphFile)
      file.close
      @communityFile = `cat output/graph.dot | ../Bin/community -i:/dev/stdin -o:/dev/stdout | grep -v "#"`
      createCommunities()
    end
    
    addRemoveNodesAndEdges()
    @i += 1
    system 'echo "Wait for 5 seconds"'
    system 'sleep 5'
  end
  
  def createCommunities()
    puts "Creating Communities"
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
#        Rubigraph.setPoolSize(1)
        @graph.removeEdge(info[1], info[2])
        @graph.removeNode(info[1])
        @graph.removeNode(info[2])
      end
    end
  end
  
  def finish()
    puts "Finalizing"
    @i = 0
    return true
  end

end
