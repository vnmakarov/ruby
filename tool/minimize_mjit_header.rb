# Copyright (C) 2017 Vladimir Makarov, <vmakarov@redhat.com>
# This is a script to minimize a C header file necessary for
# compilation of bytecode.  It is done by repetitive removing
# declarations and checking that it is still a correct version of C
# code.

# The script reads the file from stdin and put the result into stdout.

require 'mjit_header'

$code = "" # Current version of the header file.

# Read file KEEP_FNAME and return hash containing identifiers and
# array containing regexps in the file
def get_keep_info(keep_fname)
  ids = []
  rs = []
  ident_regex = /\w+/
  open(keep_fname) do |f|
    str = f.read
    str.gsub!(/[[:blank:]]*#.*$/, " ") # remove comments
    while true
      md = /\A\s*(#{ident_regex})\s*/.match(str)
      if md
        ids << md[1]
      else
        md = /\A\s*\/(.*)\/\s*/.match(str)
        break if !md
        begin
          rs << /#{md[1]}/
        rescue
          STDERR.puts "keep file #{keep_fname} contains wrong regexp: #{str[0,30]}"
        end
      end
      str.slice!(0, md.end(0))
    end
    if !str.empty?
      STDERR.puts "keep file #{keep_fname} contains garbage: #{str[0,30]}"
    end
  end

  h = {}
  ids.each {|e| h[e] = e}
  return h, rs
end

# Return true if name of declaration in DECLS is in hash KEEP_HASH
def decl_names_in?(decls, keep_hash)
  stop_pos = decls.length - 1
  while true
    pos_range = get_decl(decls, stop_pos, 1)
    return false if pos_range.end < 0
    decl_name = get_decl_name(decls[pos_range.begin..pos_range.end])
    return true if keep_hash.has_key? decl_name
    stop_pos = pos_range.begin - 1;
  end
end

# Return true if CC with CFLAGS compiles successfully the current code.
# Use TFNAME as a temporary file name.  Use PREFIX in the message in
# case of a compilation failure
def check_code(cc, cflags, tfname, prefix)
  File.open(tfname, "w") do |test|
    test.puts $code
  end
  if !system("#{cc} #{cflags} #{tfname} 2>/dev/null")
    STDERR.puts "error in #{prefix} header file:"
    STDERR.puts " Try '#{cc} #{cflags} #{tfname}'"
    exit 1
  end
end

if ARGV.length != 2
  STDERR.puts "Usage: <c-compiler> <file containing definition identifiers to keep> < in > out"
  exit 0
end

cc = ARGV[0]
keep_fname = ARGV[1]
$code = STDIN.read;
start_time = Time.now;

keep_hash, keep_rs = get_keep_info ARGV[1]

# Find start of the code we should not change:
start_invariant = get_decl($code, $code.length - 1, 1, "get_temp_addr")
# STDERR.puts "Invariant start: #{$code[start_invariant]}"
# STDERR.puts "--------------------------"
removed_decls_num = all_decls_num = 0
tfnbase = "_mjit_header_minimized_file.";
tfname = tfnbase + "c" # File we use for the compilations
factor = 2             # Factor to increase/decrease number of simultaneously processed decls
dot_factor = 50        # How many processed decls for a dot in a progress line
compilations_num = 0;
stop_pos = start_invariant.begin - 1
cflags = "-S -DMJIT_HEADER -fsyntax-only -Werror=implicit-function-declaration -Werror=implicit-int -Wfatal-errors"

# Check initial file correctness
check_code(cc, cflags, tfname, "initial")

while true
  pos_range = 0..-1
  n = factor # We try to remove N decls at once to speed up the process
  while true
    pos_range = get_decl($code, stop_pos, n)
    if pos_range.end < 0
      break if n == 1
      n /= factor # Fail: decrease searched decls number
      next
    end
    decl = $code[pos_range]
    keep_p = decl_names_in?(decl, keep_hash) || keep_rs.any? {|r| r.match(decl)}
    if !keep_p
      File.open(tfname, "w") do |test|
        test.puts $code[0...pos_range.begin], $code[pos_range.end+1..-1] # Put all but decls found above
      end
    end
    before = all_decls_num / dot_factor
    if keep_p || !system("#{cc} #{cflags} #{tfname} 2>/dev/null")
      if n != 1
        n /= factor; # Fail: decrease searched decls number
      else
        all_decls_num += 1
        # STDERR.puts "Keeping: #{decl}"
        stop_pos = pos_range.begin - 1 # Don't remove the necessary last decl
        # Always try two decls first to exclude considering one decl
        # 'ident' in something like 'typedef struct ... {} ident;'
        n = factor if stop_pos >= 0 && $code[stop_pos]==';'
      end
    else
      # STDERR.puts "Removing: #{decl}"
      # Remove unnecessary code and continue from the place right
      # before the removed code.
      $code[pos_range] = ""
      stop_pos = pos_range.begin - 1
      all_decls_num += n; removed_decls_num += n; n *= factor  # Success: increase searched decls number
    end
    compilations_num += 1
    STDERR.print "." if all_decls_num / dot_factor > before
  end
  break if pos_range.end < 0 # We processed all
end
  
STDERR.puts "\nTransforming external function to static:"

stop_pos = start_invariant.begin - 1
func_header_regex = get_func_header_regex
extern_name_hash = {}
func_def_nums = 0
while true
  pos_range = get_decl($code, stop_pos, 1)
  break if pos_range.end < 0
  stop_pos = pos_range.begin - 1
  decl = $code[pos_range]
  decl_name = get_decl_name(decl)
  if  extern_name_hash.has_key?(decl_name) && (decl =~ /#{func_header_regex};/)
    decl.sub!(/extern|static|inline/, "")
    STDERR.puts "warning: making declaration of '#{decl_name}' static inline:"
    $code[pos_range] = "static inline " + decl
  elsif md = /#{func_header_regex}{/.match(decl)
    func_def_nums += 1
    if  md[0] !~ /static/
      extern_name_hash[decl_name] = decl_name
      header = md[0]
      decl[md.begin(0)...md.end(0)] = ""
      if decl =~ /static/
        STDERR.puts "warning: a static decl inside external definition of '#{decl_name}'"
      end
      header.sub!(/extern|inline/, "")
      STDERR.puts "warning: making external definition of '#{decl_name}' static inline:"
      $code[pos_range] = "static inline " + header + decl
    end
  end
end

# Check the final file correctness
check_code(cc, cflags, tfname, "final")

# Remove temporary used files
begin
  File.delete tfname
  File.delete tfnbase + "s"
rescue
end

STDERR.puts "\n+++#{compilations_num} compilations for #{Time.now - start_time} sec: " +
            "Keeping #{all_decls_num - removed_decls_num} decls " +
            "(removed #{removed_decls_num} out of #{all_decls_num})" +
            "\n+++   #{func_def_nums} kept function definitions"

puts $code # Output the result
