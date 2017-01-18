-- Find the sum of all numbers which are equal to the sum of the factorial of their digits.
digits a 
  | a < 10    = [a]
  | otherwise = (a `mod` 10) : digits (a `div` 10)

fact' n = product [1..n]
facts = map fact' [0..9]
fact = (facts !!)
sumFact = sum . map fact . digits

isInteresting n = sumFact n == n

-- Since 9! is only six digits and 9!*6 is only seven, we only need to care about numbers up to 7 digits.
main = print . sum . filter isInteresting $ [3..10000000]
