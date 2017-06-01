# Reading an attribute through accessor
class C
  attr_reader :a
  def initialize
    @a = 1
  end
end

o = C.new

def l o
  i = 0
  while i < 1000000
    o.a
    i += 1
  end
end

i = 0
while i < 1000
  l o
  i += 1
end
