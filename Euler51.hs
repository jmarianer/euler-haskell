import Data.List
import qualified Data.Map as Map
import Data.Ord
import Primes

{-  This is too slow and also possibly incorrect
digits :: [Maybe Int]
digits = Nothing : map Just [0..9]
numbersPerhapsWithStars = map (:[]) digits ++ [d:ds | ds<-numbersWithStars, d<-digits]
numbersWithStars = filter (elem Nothing) numbersPerhapsWithStars

toNumber :: [Maybe Int] -> Int -> Int
toNumber [] _ = 0
toNumber (Just d:ds)  def = d   + 10 * (toNumber ds def)
toNumber (Nothing:ds) def = def + 10 * (toNumber ds def)

family :: [Maybe Int] -> [Int]
family ds = map (toNumber ds) [0..9]

primeFamily :: [Maybe Int] -> [Int]
primeFamily = filter isPrime . family

countPrimes :: [Maybe Int] -> Int
countPrimes = length . primeFamily

earliestLargeFamily :: Int -> [Maybe Int]
earliestLargeFamily k = head . filter ((>k) . countPrimes) $ numbersWithStars

printPrimeFamily :: [Maybe Int] -> IO ()
printPrimeFamily ds = do
  putStrLn . toString $ ds
  mapM_ print $ primeFamily ds
-}

-- We represent a family as a list of Maybe Int. [Just 5, Nothing, Just 1] represents the family 1*5.
toString' :: [Maybe Int] -> String
toString' [] = ""
toString' (x:xs) = toChar x : toString' xs
  where toChar (Just d) = head . show $ d
        toChar Nothing = '*'

toString = reverse . toString'

digits :: Int -> [Int]
digits 0 = []
digits x = (x `mod` 10) : (digits $ x `div` 10)

-- All the families that a given number belongs to, assuming stars are replaced by a given digit
families' :: Int -> Int -> [[Maybe Int]]
families' n k
  | k `elem` digits n = filter (elem Nothing) $ families'' (digits n) k
  | otherwise         = []

families'' :: [Int] -> Int -> [[Maybe Int]]
families'' [] _ = [[]]
families'' (d:ds) k
  | d == k    = addJust ++ addNothing
  | otherwise = addJust
  where addJust = map (Just d:) $ families'' ds k
        addNothing = map (Nothing:) $ families'' ds k

families :: Int -> [[Maybe Int]]
families n = concatMap (families' n) [0..9]

countSet :: Ord a => [a] -> Map.Map a Int
countSet = Map.fromListWith (+) . map (\a -> (a,1))


