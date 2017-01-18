import Data.List

threes x = (take 3 x) : (threes . tail $ x)
divisors = [2, 3, 5, 7, 11, 13, 17]

listToNum = listToNum' 0

listToNum' acc [] = acc
listToNum' acc (x:xs) = listToNum' (acc * 10 + x) xs

divides k n = n `mod` k == 0

isNice = all id . zipWith divides divisors . map listToNum . tail . threes

main = print . sum . map listToNum . filter isNice . permutations $ [0..9]

