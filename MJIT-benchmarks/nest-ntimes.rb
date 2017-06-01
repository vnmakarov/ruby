n = Integer(ARGV.shift || 18)
def a n
  x = 0
  n.times do
    n.times do
      n.times do
        n.times do
          n.times do
            n.times do
              x += 1
            end
          end
        end
      end
    end
  end
  x
end

x = 0
n.times {
  x = a n
}

puts x
