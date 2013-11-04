#!/usr/bin/env ruby
load 'Helpers/plotEvolutionHelper.rb'

i = 0
graph = PlotHelper.new()
system 'rm -f output/dump.dimacs'
system 'rm -f output/edgeTypeCountData.txt'
system 'rm -f output/removedEdgeTypeCountData.txt'

file = File.open(ARGV[0], "r")
while (line = file.gets)
  if "#{line}".include? "$"
    system 'cat output/dump.dimacs | ../Haskell/Bcp | ../Haskell/Graph variable > output/graph' + i.to_s + '.dot'
    system 'cat output/graph' + i.to_s + '.dot | ../Bin/community -i:/dev/stdin -o:/dev/stdout | grep -v "#" > output/communityMapping.dot'
    
    if i == 0
      system 'diff /dev/null output/graph' + i.to_s + '.dot > output/addRemoveNodesAndEdges.dot'
    else
      system 'diff output/graph' + (i - 1).to_s + '.dot output/graph' + i.to_s + '.dot > output/addRemoveNodesAndEdges.dot'
      system 'rm -f output/graph' + (i - 1).to_s + '.dot'
    end
    
    system 'echo "-- Pass ' + i.to_s + ' --"'
    graph.workOnDiff()
    graph.populateDiffInformation(i)
    
    graph.clear()
    graph.createCommunities()
    graph.createNodesAndEdges(i)
    graph.populateCommunityInformation(i)
    system 'rm -f output/dump.dimacs'
    i += 1
  else
    system 'echo -n "' + "#{line}" + '" >> output/dump.dimacs'
  end
end
file.close

system 'gnuplot -persist GnuplotScripts/CommunityVsIntercommunityTotal.gnu'
system 'gnuplot -persist GnuplotScripts/CommunityVsIntercommunityRemoved.gnu'
