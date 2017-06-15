#
# Script printing MD tables containing FPS and CPU time speedup of
# OPTCARROT in comparison with the first Ruby.  It also prints peak
# memory consumption scaled to the first Ruby.
#
# Usage: optcarrot-compare.rb "<optcarrot options and script>" <ruby name>:<path to the ruby> ... 
#

def print_header(commands, mt, mc)
  print "|".ljust(mt, " ")
  commands.each do |e|
    print " | ", e[0].ljust(mc, " ")
  end
  print " |\n", ":".ljust(mt + 1, "-")
  commands.each do |e|
    print "|", ":".rjust(mc + 2, "-")
  end
  print "|\n"
end

if ARGV.length < 2
  puts 'Usage optcarrot-compare.rb "<optcarrot options and script>" <ruby name>:<path to the ruby> ...'
  exit 1
end

test = ARGV[0]

commands = ARGV[1..-1].map do |e|
  if e =~ /:/
    e.split(/:/, 2)
  else
    [e, e]
  end
end

N = 2 # how many times to run a test

mt = 8
mc = commands.map {|e| e[0].length}.max
mc = 6 if mc < 6
puts "FPS:"
print_header(commands, mt, mc)
rest = []
speedup = Array.new(commands.length, 1.0)
print "FPS".ljust(mt, " ")
base = 0.0
commands.each_index do |ci|
  command = commands[ci]
  max_fps = min_elapsed = min_cpu = min_peak = 0.0
  N.times do |i|
    IO.popen({"OMR_JIT_OPTIONS" => "-Xjit"}, ["/usr/bin/time", *command[1].split, *test.split], :err => [:child, :out]) do |io|
      result = io.read
      if md = /fps: (\d+[.]\d+)/.match(result)
        fps = result[md.begin(1)...md.end(1)].to_f
        if md = /(\d+.\d+)user\s+\d+.\d+system\s+(\d+):(\d+.\d+)elapsed.+\(.+\s+(\d+)maxresident\)/.match(result)
          cpu = result[md.begin(1)...md.end(1)].to_f
          elapsed = result[md.begin(2)...md.end(2)].to_i * 60.0 + result[md.begin(3)...md.end(3)].to_f
          peak = result[md.begin(4)...md.end(4)].to_i
          if i == 0 || fps > max_fps
            max_fps, min_elapsed, min_cpu, min_peak = fps, elapsed, cpu, peak
          end
        end
      end
    end
  end
  base = max_fps if rest.length == 0
  if $?.to_i == 0
    speedup[ci] = max_fps / base
    print " | ", max_fps.round(2).to_s.ljust(mc, " ")
    rest << [min_cpu, min_peak]
  else
    speedup[ci] = 0.0
    print " | ", "-".ljust(mc, " ")
    rest << ["-", "-"]
  end
end
print " |\n"

def print_speedup(n, speedup, mt, mc)
  print "Speedup.".ljust(mt, " ")
  n.times do |i|
    print " | ", (speedup[i] == 0.0 ? "-" : speedup[i].round(2).to_s).ljust(mc, " ")
  end
  print " |\n"
end

print_speedup(commands.length, speedup, mt, mc)

2.times do |i|
  print "\n"
  puts i == 0 ? "CPU time:" : "Peak Memory"
  print_header(commands, mt, mc)
  print (i == 0 ? "Speedup" : "Peak Mem").ljust(mt, " ")
  rest.each_index do |ci|
    e = rest[ci]
    print " | "
    base = rest[0][i]
    if base == "-" || e[i] == "-"
      print "-".ljust(mc, " ")
    else
      factor = i == 0 ? base / e[i] : (e[i] + 0.0) / base
      print factor.round(2).to_s.ljust(mc, " ")
    end
  end
  print " |\n"
end
