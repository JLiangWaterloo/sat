#!/usr/bin/env ruby
DIR_NAME_MODULARITY_PLOT_HELPER = File.expand_path File.dirname(__FILE__)

class ModularityPlotHelper

  def initialize(dir_name)
    @path = DIR_NAME_MODULARITY_PLOT_HELPER + '/../output/' + dir_name + '/'
    system 'rm -f ' + DIR_NAME_MODULARITY_PLOT_HELPER + '/../output/modularityEvolutionData.txt'
    @i = 0
  end
  
  def work()
    puts '  Applying Graph, and Snap'
    @time1 = Time.now
    
    system 'cat ' + @path + 'dump.dimacs | ' + DIR_NAME_MODULARITY_PLOT_HELPER + '/../CommunityOutputOnlyModularity >> ' + @path + 'modularityEvolutionData.txt'
    system 'echo "" >> ' + @path + 'modularityEvolutionData.txt'
    
    @i += 1
  end
  
  def finish()
    puts "Finalizing"
    @i = 0
    system 'cp ' + @path + 'modularityEvolutionData.txt ' + DIR_NAME_MODULARITY_PLOT_HELPER + '/../output/modularityEvolutionData.txt'
    system 'gnuplot -persist ' + DIR_NAME_MODULARITY_PLOT_HELPER + '/../GnuplotScripts/ModularityEvolution.gnu'
  end
end
