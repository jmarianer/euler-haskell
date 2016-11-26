-- Find the smallest common multiple of [1..20]
import Data.Maybe
import qualified Data.Map as Map

ints = 1:map (+1) ints
mult k n = (n `mod` k) == 0

{- This is too slow...
 - mult k n = (n `mod` k) == 0
 - multThru k n = all (\i -> mult i n) (take k ints)
 - main = print (head (filter (multThru 20) ints))
 -}

factors n = filter (\k -> (n `mod` k) == 0) (takeWhile (<=n) ints)
smallestFactor n = (factors n) !! 1
primeFactors n = if k == n then [n] else k:primeFactors (n `quot` k)
  where k = smallestFactor n

-- (counts n) is a map of primes to the number of times they appear in the factorization of n
counts xs = foldl addToCounts Map.empty xs
  where addToCounts map x = Map.insert x (lookupOrZero + 1) map
          where lookupOrZero = fromMaybe 0 (Map.lookup x map)
addCounts = Map.unionWith max
allFactors = foldl addCounts Map.empty (map (counts.primeFactors) (tail (take 20 ints)))
totProd = foldl (*) 1 (map (\(x,y) -> x^y) (Map.toList allFactors))
main = print totProd
