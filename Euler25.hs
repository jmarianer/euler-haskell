-- Find the index of the first Fibonacci number >=10^1000.
import Data.List

fibs = 0:1:zipWith (+) fibs (drop 1 fibs)
digitCount = map (length.show) fibs

main = print (findIndex (>= 1000) digitCount)
