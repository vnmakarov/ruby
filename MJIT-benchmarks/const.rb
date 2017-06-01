RED = :red
def a
  i = 0
  while i < 20_000_000
    i += 1
    RED
  end
end

i = 0
while i < 200
  i += 1
  a
end
