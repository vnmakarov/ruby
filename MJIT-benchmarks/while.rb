def a
  i = 0
  while i < 1000_000
    i += 1
  end
end

i = 0
while i < 2000
  a
  i += 1
end
