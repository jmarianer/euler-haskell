-- Count the circular primes <1M.

-- Sieve of Eratosthenes
mult n k = (n `mod` k) == 0
sqrt' = floor . sqrt . fromInteger
isPrime 1 = False
isPrime 2 = True
isPrime n = all not $ map (mult n) $ takeWhile (<= sqrt' n) $ primes
primes = filter isPrime [2..]

-- divisor n is a power of 10 with the same number of digits as n
-- e.g., divisor 3 = 1, divisor 12345 = 10000
divisor n
  | n < 10    = 1
  | otherwise = 10 * divisor (n `div` 10)

rotateOnce n = n `div` 10 + (divisor n) * (n `mod` 10)
allRotations n = n : (takeWhile (/= n) . tail . iterate rotateOnce $ n)

isInteresting = all isPrime . allRotations

main = print . length . filter isInteresting $ [1..1000000]
