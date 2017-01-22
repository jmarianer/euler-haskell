import Data.List

-- Sieve of Eratosthenes
mult n k = (n `mod` k) == 0
sqrt' :: Integral a => a -> Int
sqrt' = floor . sqrt . fromIntegral
isPrime 1 = False
isPrime 2 = True
isPrime n = all not $ map (mult n) $ takeWhile (<= sqrt' n) $ primes
primes = filter isPrime [2..]

sumsOfPrimes = takeWhile (not . null) $ map (takeWhile (<1000000) . sumsOfPrimesWithLength) [1..]
sumsOfPrimesWithLength k = map (\n -> sum . take k . drop n $ primes) [0..]

main = print . last . filter isPrime . concat $ sumsOfPrimes
