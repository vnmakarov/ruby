# Reading an attribute through accessor
class C
  attr_reader :a
  def initialize
    @a = 1
  end
  def l
    i = 0
    while i < 1000000
      i += @a
    end
  end
end

o = C.new

i = 0
while i < 2000
  o.l
  i += 1
end
