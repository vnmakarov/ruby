def sieve(m)
  flags = Flags.dup[0,m]
  count = 0
  pmax = m - 1
  p = 2
  while p <= pmax
    unless flags[p].zero?
      count += 1
      mult = p
      while mult <= pmax
        flags[mult] = 0
        mult += p
      end
    end
    p += 1
  end
  count
end

n = 9 # (ARGV[0] || 2).to_i
Flags = ("\x1" * ( 2 ** n * 10_000)).unpack("c*")

n.downto(n-2) do |exponent|
  break if exponent < 0
  m = (1 << exponent) * 10_000
  count = 0
  20.times {count = sieve(m)}
  printf "Primes up to %8d %8d\n", m, count
end
