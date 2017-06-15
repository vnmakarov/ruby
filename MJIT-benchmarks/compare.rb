#
# Script printing MD tables containing Elapsed and CPU time speedup in
# comparison with the first Ruby.  It also prints peak memory
# consumption scaled to the first Ruby.
#
# Usage: compare.rb "<tests separated by spaces>" <ruby name>:<path to the ruby> ... 
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
  puts 'Usage compare.rb "<tests separated by spaces>" <ruby name>:<path to the ruby> ...'
  exit 1
end

tests = ARGV[0].split

commands = ARGV[1..-1].map do |e|
  if e =~ /:/
    e.split(/:/)
  else
    [e, e]
  end
end

N = 3 # how many times to run a test

mt = tests.map {|e| e.length}.max
mt = 8 if mt < 8
mc = commands.map {|e| e[0].length}.max
mc = 6 if mc < 6
puts "Elapsed time:"
print_header(commands, mt, mc)
rest = []
product = Array.new(commands.length, 1.0)
tests.each do |test|
  print test.ljust(mt, " ")
  rest << []
  base = 0
  commands.each_index do |ci|
    command = commands[ci]
    min_elapsed = 0.0
    min_cpu = 0.0
    min_peak = 0.0
    N.times do |i|
      IO.popen({"OMR_JIT_OPTIONS" => "-Xjit"}, ["/usr/bin/time", *command[1].split, test], :err => [:child, :out]) do |io|
        result = io.read
        if md = /(\d+.\d+)user\s+\d+.\d+system\s+(\d+):(\d+.\d+)elapsed.+\(.+\s+(\d+)maxresident\)/.match(result)
          cpu = result[md.begin(1)...md.end(1)].to_f
          elapsed = result[md.begin(2)...md.end(2)].to_i * 60.0 + result[md.begin(3)...md.end(3)].to_f
          peak = result[md.begin(4)...md.end(4)].to_i
          if i == 0 || elapsed < min_elapsed
            min_elapsed, min_cpu, min_peak = elapsed, cpu, peak
          end
        end
      end
    end
    base = min_elapsed if rest[-1].length == 0
    if $?.to_i == 0
      factor = base / min_elapsed
      product[ci] *= factor
      print " | ", factor.round(2).to_s.ljust(mc, " ")
      rest[-1] << [min_cpu, min_peak]
    else
      product[ci] = 0.0
      print " | ", "-".ljust(mc, " ")
      rest[-1] << ["-", "-"]
    end
  end
  print " |\n"
end

def print_geomean(n, product, power, mt, mc)
  print "Geomean.".ljust(mt, " ")
  n.times do |i|
    if product[i] == 0.0
      print " | ", "-".ljust(mc, " ")
    else
      print " | ", (product[i] ** power).round(2).to_s.ljust(mc, " ")
    end
  end
  print " |\n"
end

print_geomean(commands.length, product, 1.0 / tests.length, mt, mc)

2.times do |i|
  print "\n"
  puts i == 0 ? "CPU time:" : "Peak Memory"
  print_header(commands, mt, mc)
  product = Array.new(commands.length, 1.0)
  rest.each_index do |ei|
    print tests[ei].ljust(mt, " ")
    rest[ei].each_index do |ci|
      e = rest[ei][ci]
      print " | "
      base = rest[ei][0][i]
      factor = 0.0
      if base == "-" || e[i] == "-"
        print "-".ljust(mc, " ")
      elsif i == 0
        factor = base / e[i]
        print factor.round(2).to_s.ljust(mc, " ")
      else
        factor = (e[i] + 0.0) / base
        print factor.round(2).to_s.ljust(mc, " ")
      end
      product[ci] *= factor
    end
    print " |\n"
  end
  print_geomean(commands.length, product, 1.0 / tests.length, mt, mc)
end
