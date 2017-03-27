# Copyright (C) 2016 Vladimir Makarov, <vmakarov@redhat.com>
# This is a script to minimize a C header file necessary for
# compilation of bytecode.  It is done by repetitive removing
# declarations and checking that it is still a correct version of C
# code.

# The script reads the file from stdin and put the result into stdout.

$code = "" # Current version of the header file.

# Return star..stop of last (N>=1) decls in the code ending START
# and starting with string IDENT (if not nil).
def get_decl(stop, n, ident = nil)
  level = curr = start = 0;

  stop.downto(0) do |i|
    if level == 0 && ($code[i] == ';' || $code[i] == '}')
      start = i
      curr += 1 if stop != start
      return start + 1..stop if ident == nil && curr == n ||
                                ident != nil &&
                                /\A[^{]*#{ident}/ =~ $code[start + 1..stop]
      level += 1 if $code[i] == '}'
    elsif $code[i] == '}'
      level += 1
    elsif $code[i] == '{'
      level -= 1
    end
  end
  return 0..-1
end

if ARGV.length != 1
  STDERR.puts "Usage: <c-compiler> < in > out"
  exit 0
end

cc = ARGV[0]
$code = STDIN.read;
start_time = Time.now;

# Find start of the code we should not change:
start_invariant = get_decl $code.length - 1, 1, "get_temp_addr"
# STDERR.puts "Invariant start: #{$code[start_invariant]}"
# STDERR.puts "--------------------------"
removed = all = 0
tfnbase = "_mjit_header_minimized_file.";
tfname = tfnbase + "c" # File we use for the compilation
factor = 2             # Factor to increase/decrease number of simultaneously processed decls
dot_factor = 50    # How many processed decls for a dot in a progress line
ncomps = 0;
stop = start_invariant.begin - 1
while true
  pos = 0..-1
  n = factor # We try to remove N decls at once to speed up the process
  while true
    pos = get_decl stop, n
    if pos.end < 0
      break if n == 1
      n /= factor # Fail: decrease searched decls number
      next
    end
    File.open(tfname, "w") do |test|
      test.puts $code[0...pos.begin], $code[pos.end+1..-1] # Put all but decls found above
    end
    before = all / dot_factor
    if !system("#{cc} -S -DMJIT_HEADER -Werror=implicit-function-declaration -Werror=implicit-int -Wfatal-errors #{tfname} 2>/dev/null")
      if n != 1
        n /= factor; # Fail: decrease searched decls number
      else
        all += 1
        # STDERR.puts "Keeping: #{$code[pos]}"
        stop = pos.begin - 1; # Don't remove the necessary last decl
        # Always try two decls first to exclude considering one decl
        # 'ident' in something like 'typedef struct ... {} ident;'
        n = factor if stop >= 0 && $code[stop]==';'
      end
    else
      # STDERR.puts "Removing: ", $code[pos]
      # Remove unnecessary code and continue from the place right
      # before the removed code.
      $code[pos] = ""
      stop = pos.begin - 1
      all += n; removed += n; n *= factor  # Success: increase searched decls number
    end
    ncomps += 1
    STDERR.print "." if all / dot_factor > before
  end
  break if pos.end < 0 # We processed all
end
  
STDERR.puts
begin
  File.delete tfname
  File.delete tfnbase + "s"
rescue
end
STDERR.puts "#{ncomps} compilations for #{Time.now - start_time} sec: Removed #{removed} decls out of #{all}"
puts $code # Output the result
