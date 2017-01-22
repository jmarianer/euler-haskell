import Data.List
import Data.Ratio

diagDiffs = 2:2:2:2 : map (+2) diagDiffs
diags = 1 : zipWith (+) diagDiffs diags

-- Sieve of Eratosthenes
mult n k = (n `mod` k) == 0
sqrt' :: Integral a => a -> Int
sqrt' = floor . sqrt . fromIntegral
isPrime 1 = False
isPrime 2 = True
isPrime n = all not $ map (mult n) $ takeWhile (<= sqrt' n) $ primes
primes = filter isPrime [2..]

diagsPrime = map isPrime diags

-- n is half the side of the square (rounded down)
primesAlongDiags n = length . filter id . take (4*n+1) $ diagsPrime
totalDiagLength n = (4*n+1)

ratio n = primesAlongDiags n % totalDiagLength n

main = print . (\n -> 2*n+1) . head . filter (\n -> ratio n < 1%10) $ [1..]
