-- Find the 10001st prime number

ints = 1:map (+1) ints
mult k n = (n `mod` k) == 0

-- Sieve of Eratosthenes
isPrime 2 = True
isPrime n = all not (map (\k -> mult k n) (takeWhile (<= (floor (sqrt (fromInteger n)))) primes))
primes = filter isPrime (tail ints)

main = print (primes !! 10000)
