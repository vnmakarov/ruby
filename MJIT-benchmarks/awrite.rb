# Writting an attribute through accessor
class C
  attr_writer :a
  def initialize
    @a = 1
  end
end

o = C.new

def w o
  i = 0
  while i < 1000000
    o.a = i
    i += 1
  end
end

i = 0
while i < 1000
  w o
  i += 1
end
