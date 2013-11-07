#!/usr/bin/env ruby
load 'Helpers/GraphBuilder.rb'
load 'Helpers/graphvizHelper.rb'
load 'Helpers/ubigraphHelper.rb'
load 'Helpers/edgePlotHelper.rb'

class GraphConstructor

  #
  # Constants
  #
  THREAD_COUNT = 16
  
  def initialize(type, details, filename, file_type)
    system 'mkdir -p output'
    system 'rm -f output/dump.dimacs'
  
    if type == "graphviz"
      @graph = GraphvizHelper.new(filename, file_type, details)
    elsif type == "ubigraph"
      @graph = UbigraphHelper.new()
    elsif type == "plot"
      @graph = EdgePlotHelper.new()
    else
      puts "Wrong type was entered. Must be either graphviz, ubigraph, or plot."
    end    
  end
  
  def work(file)
    done = false
    puts "Reading file"
    @time1 = Time.now
    
    File.open(file).each do |line|
      if "#{line}"[0,1] == "$"
        printTime()
        @graph.work()
        system 'rm -f output/dump.dimacs'
        done = true
      else
        system 'echo -n "' + "#{line}" + '" >> output/dump.dimacs'
        done = false
      end
    end
    
    if !done
      @graph.work()
      system 'rm -f output/dump.dimacs'
    end
  end
  
  def finish()
    @graph.finish()
  end
  
  def printTime()
    time2 = Time.now
    puts "Time = " + ((time2 - @time1)).to_s + "s"
    @time1 = time2
  end

end
