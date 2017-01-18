import Data.List

digits n
  | n < 10    = [n]
  | otherwise = digits (n `div` 10) ++ [n `mod` 10]

mult n k = (n `mod` k) == 0
sqrt' = floor . sqrt . fromInteger
isPrime 1 = False
isPrime 2 = True
isPrime n = all not $ map (mult n) $ takeWhile (<= sqrt' n) $ primes
primes = filter isPrime [2..]

isPandigital :: Integer -> Bool
isPandigital n = (sort . digits $ n) == [1..fromIntegral . length . digits $ n]

-- The number can't be an 8- or 9-digit pandigital because they are all divisible by 9.
main = print . head . filter isPrime . filter isPandigital . reverse $ [1..7654321] -- . takeWhile (<7654321) $ primes
