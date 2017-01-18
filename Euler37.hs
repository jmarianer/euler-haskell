-- Find the sum of all eleven truncatable primes.

-- Sieve of Eratosthenes
mult n k = (n `mod` k) == 0
sqrt' = floor . sqrt . fromInteger
isPrime 1 = False
isPrime 2 = True
isPrime n = all not $ map (mult n) $ takeWhile (<= sqrt' n) $ primes
primes = filter isPrime [2..]

leftTruncations n
  | n < 10    = [n]
  | otherwise = n : leftTruncations (n `div` 10)

divisor n
  | n < 10    = 1
  | otherwise = 10 * divisor (n `div` 10)

rightTruncations n
  | n < 10    = [n]
  | otherwise = n : rightTruncations (n `mod` (divisor n))

isInteresting n = all isPrime (leftTruncations n ++ rightTruncations n)

main = print . sum . take 11 . filter isInteresting . drop 4 $ primes
