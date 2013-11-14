#!/usr/bin/env ruby
load 'Helpers/GraphBuilder.rb'
load 'Helpers/graphvizHelper.rb'
load 'Helpers/ubigraphHelper.rb'
load 'Helpers/edgePlotHelper.rb'
load 'Helpers/modularityPlotHelper.rb'

class GraphConstructor

  def initialize(type, details, output_type, filename, output_format)
    puts type
    puts details
    puts output_type
    puts filename
    puts output_format
    @init_time = Time.now
    dir_name = File.basename( filename, ".*" )
    @dump_path = 'output/' + dir_name + '/dump.dimacs'
    system 'rm -rf output/' + dir_name
    system 'mkdir -p output/' + dir_name
  
    if type == "graphviz"
      @graph = GraphvizHelper.new(dir_name, output_type, details, output_format)
    elsif type == "ubigraph"
      @graph = UbigraphHelper.new(dir_name, output_type)
    elsif type == "edgeplot"
      @graph = EdgePlotHelper.new(dir_name)
    elsif
      @graph = ModularityPlotHelper.new(dir_name)
    else
      puts "Wrong type was entered. Must be either graphviz, ubigraph, edgeplot, or modularity."
    end    
  end
  
  def work(file)
    done = false
    puts "Reading file"
    @time1 = Time.now
    
    dump = File.open(@dump_path, "w")
    File.open(file).each_slice(1000) do |lines|
      if lines.include?("$\n")
        dump = seperateLines(lines, dump)
        done = true
      else
        dump.write(lines.join)
        done = false
      end
    end
    
    if !done
      dump.close
      @graph.work()
    end
  end
  
  def seperateLines(lines, dump)
    i = lines.index("$\n")
    if i > 0
      dump.write(lines[0,i].join)
    end
        
    printTime("  Time = ", @time1)
    
    dump.close
    @graph.work()
    dump = File.open(@dump_path, "w")
    dump.truncate(0)
    
    printTime("  Time for pass = ", @time1)
    @time1 = Time.now
    puts "Reading file"
    
    if i < (lines.length - 1)
      lines = lines[i+1,lines.length]
      if lines.include?("$\n")
        dump = seperateLines(lines, dump)
      else
        dump.write(lines.join)
      end
    end
    
    return dump
  end
  
  def finish()
    @graph.finish()
    printTime("Total Run Time = ", @init_time)
  end
  
  def printTime(msg, time)
    time2 = Time.now
    puts msg + ((time2 - time)).to_s + "s"
  end

end
