def a
  i = 0; a = [42]; ind = 0
  while i < 2_000_000 do
    a[ind] = 21
    i += 1
  end
end

i = 0
while i < 1_000 do
  a
  i += 1
end
