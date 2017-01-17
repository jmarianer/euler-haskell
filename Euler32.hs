-- Find the sum of all pandigital products

{- A pandigital product must be the product of a two- and three-digit or one-
 - and four-digit number, and must itself be four digits long. -}
import Data.List

digits a = if (a < 10)
           then [a]
           else (a `mod` 10) : digits (a `div` 10)

digitsList :: [Int] -> [Int]
digitsList = sort . concat . map digits

pandigital :: [Int] -> Bool
pandigital as = (digitsList as) == [1..9]

one = [1..9]
two = [12..98]
three = [123..987]
four = [1234..9876]

prod :: [Int] -> [Int] -> [[Int]]
prod xs ys = [[x, y, x*y] | x <- xs, y <- ys]

pandigitalIdentities = filter pandigital (prod two three ++ prod one four)
pandigitalProducts = map (!! 2) pandigitalIdentities

main = print (sum (nub pandigitalProducts))
