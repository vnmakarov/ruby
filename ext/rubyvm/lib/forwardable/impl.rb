# :stopdoc:
module Forwardable
  FILTER_EXCEPTION = ""

  def self._valid_method?(method)
    iseq = RubyVM::InstructionSequence.compile("().#{method}", nil, nil, 0, false)
  rescue SyntaxError
    false
  else
    insn = iseq.to_a.dig(-1, 1)
    # the method is the 1st operand or the 2nd one in case of insn with a cont
    (insn[1].instance_of?(Hash) ? insn[1][:mid] : insn[2][:mid]) == method.to_sym 
  end

  def self._compile_method(src, file, line)
    RubyVM::InstructionSequence.compile(src, file, file, line,
               trace_instruction: false,
               tailcall_optimization: true)
      .eval
  end
end
