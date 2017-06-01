n = Integer(ARGV.shift || 19)
def a n
  x = 0
  a = 0
  while a < n do
    b = 0
    while b < n do
      c = 0
      while c < n do
        d = 0
        while d < n do
          e = 0
          while e < n do
            f = 0
            while f < n do
              x += 1
              f += 1
            end
            e += 1
          end
          d += 1
        end
        c += 1
      end
      b += 1
    end
    a += 1
  end
  x
end
  
x = 0
n.times {
  x = a n
}

puts x
