def f
  a = [:foo, 'bar', 2]
  i = 0
  while i < 10_000_000
    a.first
    i += 1
  end
end

i = 0
while i < 100
  i += 1
  f
end

