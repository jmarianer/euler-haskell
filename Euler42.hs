import Data.List
import Data.Char

getWords s = getWords' [] s

getWords' acc "" = acc
getWords' acc s = getWords' (first:acc) (drop 1 second)
  where (first, second) = break (== ',') s

dequote names = map (filter (/= '"')) names

score "" = 0
score name = (ord (head name)) - 64 + (score (tail name))

triangleNumbers = scanl (+) 0 [1..]
lowTriangles = takeWhile (<= 26*14) triangleNumbers -- 14 = max length of a word in the data file

isTriangle word = (score word) `elem` lowTriangles

main = readFile "words" >>= print . length . filter isTriangle . dequote . getWords
