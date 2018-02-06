module Primes (isPrime, primes) where

-- Sieve of Eratosthenes
mult n k = (n `mod` k) == 0

sqrt' :: Int -> Int
sqrt' = floor . sqrt . fromIntegral

isPrime' :: Int -> Bool
isPrime' n
  | n < 2 = False
  | n == 2 = True
  | otherwise = all not $ map (mult n) $ takeWhile (<= sqrt' n) $ primes

primes :: [Int]
primes = filter isPrime' [2..]

isPrime :: Int -> Bool
isPrime k = k == (head . dropWhile (<k) $ primes)
