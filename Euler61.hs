import Data.List

poly n = 1 : zipWith (+) (poly n) [n-1, 2*n-3..]

elemIncreasing a (x:xs)
  | x == a     = True
  | x > a      = False
  | otherwise  = elemIncreasing a xs

concatNum m n = m * 100 + n

{- Too slow
sextuplets = [[a `concatNum` b, b `concatNum` c, c `concatNum` d, d `concatNum` e, e `concatNum` f, f `concatNum` a]
              | a <- [10..99], b <- [10..99], c <- [10..99], d <- [10..99], e <- [10..99], f <- [10..99]]

setsRepresented xs = [n | n <- [3..8], x <- xs, x `elemIncreasing` p n]
-}

fourDigitOctagonals = takeWhile (<10000) . dropWhile (<1000) $ poly 8

-- h = upper two digits
-- ps = list of figurates
-- out = (figurates, answer)
--findPentagonal :: Int -> [Int] -> [(Int, Int)]
findPentagonal h ps = concat . map find' $ ps
  where find' p = zip (repeat p) . takeWhile (<(h+1)*100) . dropWhile (<1000) . dropWhile (<h*100) $ poly p


first = map (\n -> ([3..7], [n])) fourDigitOctagonals

addOnePentagonal foo = foo >>= next
next :: ([Integer], [Integer]) -> [([Integer], [Integer])]
next (ps, ans) = map something . findPentagonal ((last ans) `mod` 100) $ ps
  where something (p, newAns) = (delete p ps, ans ++ [newAns])

myLast = iterate addOnePentagonal first !! 4

filtered = filter g myLast
  where g ([p], ans) = (completeCircle ans > 1000) && (completeCircle ans) `elemIncreasing` (poly p)

completeCircle ans = ((head ans) `div` 100) + ((last ans) `mod` 100 * 100)

main = print (sum l + completeCircle l)
  where l = snd . head $ filtered
