#!/usr/bin/env ruby
DIR_NAME_GRAPH_CONSTRUCTOR = File.expand_path File.dirname(__FILE__)

load DIR_NAME_GRAPH_CONSTRUCTOR + '/GraphBuilder.rb'
load DIR_NAME_GRAPH_CONSTRUCTOR + '/graphvizHelper.rb'
load DIR_NAME_GRAPH_CONSTRUCTOR + '/ubigraphHelper.rb'
load DIR_NAME_GRAPH_CONSTRUCTOR + '/edgePlotHelper.rb'
load DIR_NAME_GRAPH_CONSTRUCTOR + '/modularityPlotHelper.rb'

class GraphConstructor

  def initialize(type, details, output_type, filename, output_format)
    @init_time = Time.now
    dir_name = File.basename( filename, ".*" )
    @dump_path = DIR_NAME_GRAPH_CONSTRUCTOR + '/../output/' + dir_name + '/dump.dimacs'
    system 'rm -rf ' + DIR_NAME_GRAPH_CONSTRUCTOR + '/../output/' + dir_name
    system 'mkdir -p ' + DIR_NAME_GRAPH_CONSTRUCTOR + '/../output/' + dir_name
  
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
    @i = 0
    puts '--- Pass ' + @i.to_s + ' ---'
    puts "  Reading file"
    @time1 = Time.now
    
    dump = File.open(@dump_path, "w")
    File.open(file).each_slice(10) do |lines|
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
        
    printTime("    Time = ", @time1)
    
    dump.close
    @graph.work()
    dump = File.open(@dump_path, "w")
    dump.truncate(0)
    
    printTime("  Time for pass = ", @time1)
    @i += 1
    @time1 = Time.now
    puts '--- Pass ' + @i.to_s + ' ---'
    puts "  Reading file"
    
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
