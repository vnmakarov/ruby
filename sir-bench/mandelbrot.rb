# A version w/o complex numbers which created a GC stress
def mandelbrot? x, y
  i = 0
  while i<100
    i += 1
    r = x * x - y * y
    y = 2.0 * x * y
    x = r
    return false if Math.sqrt(x * x + y * y) > 2.0
  end
  true
end

k = 0
100.times do |i|
  ary = []

  (0..1000).each{|dx|
    (0..1000).each{|dy|
      x = dx / 50.0
      y = dy / 50.0
      ary << [x, y] if mandelbrot?(x,y)
    }
  }
  k += ary.length
end

p k
