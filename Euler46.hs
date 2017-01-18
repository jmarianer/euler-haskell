-- Sieve of Eratosthenes
mult n k = (n `mod` k) == 0
sqrt' = floor . sqrt . fromInteger
isPrime 1 = False
isPrime 2 = True
isPrime n = all not $ map (mult n) $ takeWhile (<= sqrt' n) $ primes
primes = filter isPrime [2..]

-- http://stackoverflow.com/questions/4333459/is-it-square-check
isSquare x = x == head (dropWhile (< x) squares)
  where squares = scanl (+) 0 [1,3..]

isGoldbach n = any isSquareDifference $ takeWhile (<=n) primes
  where isSquareDifference k = isSquare ((n-k) `div` 2)

main = print . head . filter (not . isGoldbach) $ [3,5..]
