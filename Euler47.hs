import Data.List

-- Sieve of Eratosthenes
mult n k = (n `mod` k) == 0
sqrt' :: Integral a => a -> Int
sqrt' = floor . sqrt . fromIntegral
isPrime 1 = False
isPrime 2 = True
isPrime n = all not $ map (mult n) $ takeWhile (<= sqrt' n) $ primes
primes = filter isPrime [2..]

{-
factors = factors' [] primes
factors' acc _ 1 = acc
factors' acc (p:ps) n
  | n `mod` p == 0  = factors' (p:acc) (p:ps) (n `div` p)
  | otherwise       = factors' acc ps n
-}

factors = map (factors' primes) [0..]
factors' _ 0 = []
factors' _ 1 = []
factors' (p:ps) n
  | n `mod` p == 0  = p : factors !! (n `div` p)
  | otherwise       = factors' ps n

factorCounts = map (length . nub) factors

k = 4
numsWithKFactors = map fst . filter (\(_,f) -> f == k) . zip [0..] $ factorCounts

firstInteresting all@(n:ns)
  | take k all == [n..n-1+(fromIntegral k)]  = n
  | otherwise                    = firstInteresting ns

main = print . firstInteresting $ numsWithKFactors
--}
