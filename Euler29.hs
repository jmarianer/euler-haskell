-- Find the number of distinct terms a^b where a,b are in [2..100]
import Data.Set

main = print (length (fromList ([a^b | a <- [2..100], b <- [2..100]])))
