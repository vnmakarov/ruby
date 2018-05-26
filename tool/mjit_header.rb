# Copyright (C) 2017 Vladimir Makarov, <vmakarov@redhat.com>

# This is a coommon code for MJIT header manipulations

# Return start..stop of last (N>=1) decls in CODE ending STOP
# and starting with string IDENT (if not nil).
def get_decl(code, stop, n, ident = nil)
  level = curr = start = 0;

  stop.downto(0) do |i|
    if level == 0 && (i == 0 || code[i] == ';' || code[i] == '}')
      start = i
      curr += 1 if stop != start
      start = -1 if i == 0 && code[i] != ';' && code[i] != '}'
      return start + 1..stop if ident == nil && curr == n ||
                                ident != nil &&
                                /\A[^{]*#{ident}/ =~ code[start + 1..stop]
      level += 1 if code[i] == '}'
    elsif code[i] == '}'
      level += 1
    elsif code[i] == '{'
      level -= 1
    end
  end
  return 0..-1
end

# Return a regex describing a GCC/LLVM attribute
def get_attr_regex
  attr_value_regex = /[^()]|\([^()]*\)/
  /__attribute__\s*\(\((#{attr_value_regex})*\)\)/
end

# Return a regex describing a GCC/LLVM function header
def get_func_header_regex
  attr_regex = get_attr_regex
  /\A((#{attr_regex})|[^\[{(])*\((#{attr_regex}|[^()])*\)(\s*#{attr_regex})*\s*/
end

# Given DECL return the name of it, nil if failed
def get_decl_name(decl)
  ident_regex = /\w+/
  attr_regex = get_attr_regex
  reduced_decl = decl.gsub(/#{attr_regex}/, "") # remove attributes
  su1_regex = /{[^{}]*}/
  su2_regex = /{([^{}]|su1_regex)*}/
  su3_regex = /{([^{}]|su2_regex)*}/ # 3 nested structs/unions is probably enough
  reduced_decl.gsub!(/#{su3_regex}/, "") # remove strutcs/unions in the header
  id_seq_regex = /\s*(#{ident_regex}(\s+|\s*[*]\s*))*/
  # Process function header:
  md = /\A#{id_seq_regex}(?<name>#{ident_regex})\s*\(/.match(reduced_decl)
  return md[:name] if md
  # Process non-function declaration:
  reduced_decl.gsub!(/\s*=[^;]+(?=;)/, "") # remove initialization
  md = /#{id_seq_regex}(?<name>#{ident_regex})/.match(reduced_decl);
  return md[:name] if md
  return nil
end
