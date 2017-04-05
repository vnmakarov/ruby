# Copyright (C) 2016 Vladimir Makarov, <vmakarov@redhat.com>
#
# Extract extern declarations to use them for symbols export.
# The script reads the file from stdin and put the result into stdout.

require 'mjit_header'

$code = "" # Current version of the header file.

if ARGV.length != 0
  STDERR.puts "Usage: < in > out"
  exit 0
end

$code = STDIN.read;
start_time = Time.now;

# Find start of the code to process:
start_invariant = get_decl($code, $code.length - 1, 1, "get_temp_addr")
all_decls_num = 0
stop_pos = start_invariant.begin - 1
extern_regex = /\A[^\[{]*extern[^;{]*;/
func_header_regex = get_func_header_regex
exit_code = 0
while true
  pos_range = 0..-1
  while true
    pos_range = get_decl($code, stop_pos, 1)
    break if pos_range.end < 0
    decl = $code[pos_range.begin..pos_range.end]
    if decl =~ extern_regex || decl =~ /#{func_header_regex};/
      STDOUT.puts decl if decl !~ /typedef|static/
    elsif (md = /#{func_header_regex}{/.match(decl)) && decl !~ /static/
      STDERR.puts "non-static definition: #{md[0]}..."
      exit_code = 1
    end
    all_decls_num += 1
    stop_pos = pos_range.begin - 1
  end
  break if pos_range.end < 0 # We processed all decls
end
  
STDERR.puts "Processing #{all_decls_num} declarations for #{Time.now - start_time} sec"
exit exit_code
