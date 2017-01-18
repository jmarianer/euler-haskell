tri   = 1 : zipWith (+) tri   [2,3..]
penta = 1 : zipWith (+) penta [4,7..]
hexa  = 1 : zipWith (+) hexa  [5,9..]

elemIncreasing a (x:xs)
  | x == a     = True
  | x > a      = False
  | otherwise  = elemIncreasing a xs

intersectIncreasing (x:xs) (y:ys)
  | x == y     = x : intersectIncreasing xs ys
  | x < y      =     intersectIncreasing xs (y:ys)
  | x > y      =     intersectIncreasing (x:xs) ys


main = print . take 3 $ tri `intersectIncreasing` penta `intersectIncreasing` hexa
