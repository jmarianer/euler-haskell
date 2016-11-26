-- Print the sum of all integers <1000 that are multiples of 3 or 5

ints = 1:map (+1) ints
mult k n = (n `mod` k) == 0
mult35 n = or [mult 3 n, mult 5 n]
main = print (sum (filter (mult35) (takeWhile (<1000) ints)))
