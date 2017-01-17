-- Find the sum of scores of names in a file
import Data.List
import Data.Char

getNames s = getNames' [] s

getNames' acc "" = acc
getNames' acc s = getNames' (first:acc) (drop 1 second)
  where (first, second) = break (== ',') s

dequote names = map (filter (/= '"')) names

score "" = 0
score name = (ord (head name)) - 64 + (score (tail name))

pairProd (x,y) = x*y

main = readFile "names" >>= print.sum.(map pairProd).(zip [1..]).(map score).sort.dequote.getNames
