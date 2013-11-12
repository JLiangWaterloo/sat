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
    dir_name = File.basename( filename, ".*" )
    @dump_path = 'output/' + dir_name + '/dump.dimacs'
    system 'rm -rf output/' + dir_name
    system 'mkdir -p output/' + dir_name
  
    if type == "graphviz"
      @graph = GraphvizHelper.new(dir_name, file_type, details)
    elsif type == "ubigraph"
      @graph = UbigraphHelper.new(dir_name)
    elsif type == "plot"
      @graph = EdgePlotHelper.new(dir_name)
    else
      puts "Wrong type was entered. Must be either graphviz, ubigraph, or plot."
    end    
  end
  
  def work(file)
    done = false
    puts "Reading file"
    @time1 = Time.now
    
    dump = File.open(@dump_path, "w")
    File.open(file).each do |line|
      if "#{line}"[0,1] == "$"
        dump.close
        @graph.work()
        dump = File.open(@dump_path, "w")
        dump.truncate(0)
        done = true
        printTime()
      else
        dump.write(line)
        done = false
      end
    end
    
    if !done
      dump.close
      @graph.work()
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
