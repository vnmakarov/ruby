# Writting an attribute through accessor
class C
  attr_writer :a
  def initialize
    @a = 1
  end
  def w
    i = 0
    while i < 1000000
      @a = i
      i += 1
    end
  end
end

o = C.new

i = 0
while i < 1000
  o.w
  i += 1
end
