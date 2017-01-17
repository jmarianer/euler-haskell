-- Find the millionth permutation of 0123456789 in lexicographical order
import Data.List
import Data.Char

lexicographicPermutations :: [Int] -> [[Int]]
lexicographicPermutations [] = [[]]
lexicographicPermutations xs = concatMap (\x -> map (x:) (lexicographicPermutations (xs\\[x]))) xs

--main = print (map (map intToDigit) (lexicographicPermutations [0..2]))
main = print (map intToDigit ((lexicographicPermutations [0..9]) !! 999999))
