def a
end

def l
  i = 0
  while i < 100000 do
    a
    i += 1
  end
end

i = 0
while i < 8000 do
  l
  i += 1
end
