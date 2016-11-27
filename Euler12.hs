-- Find the smallest triangular number with over 500 divisors
import Data.Maybe
import qualified Data.Map as Map

ints = 1:map (+1) ints

{- The slow, naive way
 - triangle 1 = 1
 - triangle n = triangle (n - 1) + n
 -}
triangle n = (n * (n + 1)) `div` 2

factors n = filter (\k -> (n `mod` k) == 0) (takeWhile (<=n) ints)
{- The slow, naive way
 - countFactors = length . factors
 -}
smallestFactor n = (factors n) !! 1
primeFactors n = if k == n then [n] else k:primeFactors (n `quot` k)
  where k = smallestFactor n
counts xs = foldl addToCounts Map.empty xs
  where addToCounts map x = Map.insert x (lookupOrZero + 1) map
          where lookupOrZero = fromMaybe 0 (Map.lookup x map)
countFactors 1 = 1
countFactors n = product (map ((+1).snd) (Map.toList (counts (primeFactors n))))

main = print (head (dropWhile (\n -> countFactors n <= 500) (map triangle ints)))
