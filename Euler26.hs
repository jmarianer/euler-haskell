-- Find the number n<1000 with the longest cycle in 1/n
import Data.List
import Data.Maybe

removeFactors k n = if (n `mod` k == 0)
                    then removeFactors k (n `div` k)
                    else n
noTens = (removeFactors 2) . (removeFactors 5)
nines = 9 : (map (\x -> x * 10 + 9)) nines
factor k n = (n `mod` k == 0)

cycleLength n = 1 + fromJust (findIndex (factor (noTens n)) nines)
cycleLengths = 0 : map cycleLength [1..1000]

main = print (findIndex (== (maximum cycleLengths)) cycleLengths)
