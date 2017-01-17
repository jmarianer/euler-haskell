-- Find all the digit cancelling fractions and yield the denominator of their products
import Data.Ratio

{- A digit cancelling function has three separate digits. We'll just go over all the options -}

digit = [1..9]

-- Numerator digit, Shared digit, Denominator digit
possibleFractions :: Rational -> Rational -> Rational -> [Rational]
possibleFractions n s d = [num/denom | num <- nums, denom <- denoms ]
  where nums   = [10*n + s, 10*s + n]
        denoms = [10*d + s, 10*s + d]

isDigitCancelling n s d = filter (== n/d) (possibleFractions n s d)

allUnderOne = (filter (<1) (concat [isDigitCancelling n s d | n <- digit, s <- digit, d <- digit]))

main = print (denominator (foldr (*) 1 allUnderOne))
