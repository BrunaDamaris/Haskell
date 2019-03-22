fat:: Integer -> Integer
fat 0 = 1
fat n = n * fat(n-1)

fib:: Integer->Integer
fib 1 = 1
fib 2 = 1
fib n = fib(n-2) + fib(n-1)

somafib:: Integer -> Integer
somafib 1 = 1
somafib 2 = 1
somafib n = fib n + somafib(n-1)

patermo::Integer -> Integer -> Integer
patermo 1 r = 2
patermo n r = (patermo (n-1) r) + r

pgtermo:: Integer -> Integer -> Integer
pgtermo 1 q = 1
pgtermo n q = (pgtermo 1 q) * (q^(n-1))

pa:: Integer -> Integer -> [Integer]
pa 0 r = []
pa n r = pa (n-1) r ++ [patermo n r]

pg:: Integer -> Integer -> [Integer]
pg 0 q = []
pg n q = pg (n-1) q ++ [pgtermo n q]
