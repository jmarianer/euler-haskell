import Data.List
import Control.Monad

digits n
  | n < 10    = [n]
  | otherwise = digits (n `div` 10) ++ [n `mod` 10]

main = print . maximum . map (sum . digits) . liftM2 (^) [1..100] $ [1..100]
