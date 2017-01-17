-- Find the sum of all positive integers which cannot be written as the sum of two abundant numbersgiven that all numbers >28123 can be written as such a sum.
import Data.List
import qualified Data.Set as Set

isqrt = floor . sqrt . fromIntegral
lowFactors n = filter (\k -> (n `mod` k) == 0) [1..isqrt n]
factors n = nub (concat [[k, n `div` k] | k <- lowFactors n])
sumFactors = sum . factors
abundant n = (sumFactors n > 2*n)  -- Because n itself is included
abundants = filter abundant [1..28123]

cartSum xs ys = [x + y | x <- xs, y <- ys]
summable = Set.fromList (filter (<= 28123) (cartSum abundants abundants))
nonsummable = Set.fromList [1..28123] Set.\\ summable

main = print (sum nonsummable)
