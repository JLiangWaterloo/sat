#!/usr/bin/env ruby

class ModularityPlotHelper

  def initialize(dir_name)
    @path = 'output/' + dir_name + '/'
    system 'rm -f output/modularityEvolutionData.txt'
    @i = 0
  end
  
  def work()
    puts '--- Pass ' + @i.to_s + ' ---'
    puts '  Applying Graph, and Snap'
    @time1 = Time.now
    
    system 'cat ' + @path + 'dump.dimacs | ./CommunityOutputOnlyModularity >> ' + @path + 'modularityEvolutionData.txt'
    system 'echo "" >> ' + @path + 'modularityEvolutionData.txt'
    
    @i += 1
  end
  
  def finish()
    puts "Finalizing"
    @i = 0
    system 'cp ' + @path + 'modularityEvolutionData.txt output/modularityEvolutionData.txt'
    system 'gnuplot -persist GnuplotScripts/ModularityEvolution.gnu'
  end
end
