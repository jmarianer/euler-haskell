-- Find the sum of all primes under 2M

ints = 1:map (+1) ints
mult k n = (n `mod` k) == 0
isPrime 2 = True
isPrime n = all not (map (\k -> mult k n) (takeWhile (<= (floor (sqrt (fromInteger n)))) primes))
primes = filter isPrime (tail ints)

-- This takes a while (a few seconds), but I don't think it can be made any faster.
main = print (sum (takeWhile (<2000000) primes))
