#!/usr/bin/env ruby

class UbigraphHelper

  def initialize(dir_name, type)
    @graph = GraphBuilder.new("ubigraph")
    @i = 0
    @path = 'output/' + dir_name + '/'
    @type = type
  end
  
  def work()
    puts '--- Pass ' + @i.to_s + ' ---'
    puts 'Applying Graph, and Snap'
    @time1 = Time.now
    
    if @type == "evolution"
      system 'cat ' + @path + 'dump.dimacs | ../Haskell/Graph variable > ' + @path + 'graph' + @i.to_s + '.dot'
    else
      system 'cat ' + @path + 'dump.dimacs | ../Haskell/Bcp | ../Haskell/Graph variable > ' + @path + 'graph' + @i.to_s + '.dot'
    end
    
    if @i == 0
      system 'diff /dev/null ' + @path + 'graph' + @i.to_s + '.dot > ' + @path + 'addRemoveNodesAndEdges.dot'
      system 'cat ' + @path + 'graph' + @i.to_s + '.dot | ../Bin/community -i:/dev/stdin -o:/dev/stdout | grep -v "#" > ' + @path + 'communityMapping.dot'
      createCommunities()
    else
      system 'diff ' + @path + 'graph' + (@i - 1).to_s + '.dot ' + @path + 'graph' + @i.to_s + '.dot > ' + @path + 'addRemoveNodesAndEdges.dot'
      system 'rm -f ' + @path + 'graph' + (@i - 1).to_s + '.dot'
    end
    printTime()
    
    addRemoveNodesAndEdges()
    color()
    @i += 1
    system 'echo "Wait for 5 seconds"'
    system 'sleep 5'
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
  
  def addRemoveNodesAndEdges() 
    puts "Adding and Removing Nodes and Edges" 
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
#        Rubigraph.setPoolSize(1)
        @graph.removeEdge(info[1], info[2])
        @graph.removeNode(info[1])
        @graph.removeNode(info[2])
      end
    end
    file.close
    printTime()
  end
  
  def color()
    puts "Coloring Graph"
    @time1 = Time.now
    @graph.color()
    printTime()
  end
  
  def finish()
    puts "Finalizing"
    @i = 0
    return true
  end
  
  def printTime()
    time2 = Time.now
    puts "    Time = " + ((time2 - @time1)).to_s + "s"
    @time1 = time2
  end

end
