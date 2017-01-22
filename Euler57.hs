import Control.Monad
import Data.List
import Data.Maybe
import Data.Ratio

next :: Rational -> Rational
next n = 1 + 1/(1 + n)

expansions = iterate next 1

digitCount n = elemIndex True . map (n<) . map (10^) $ [0..]

longerNumerator rat = liftM2 (>) (digitCount p) $ digitCount q
  where p = numerator rat
        q = denominator rat

main = print . length . filter (fromJust . longerNumerator) . take 1000 $ expansions
