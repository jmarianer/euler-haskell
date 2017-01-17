-- Find the sum of all numbers that can be written as a sum of the fifth power of their digits. 1=1^5 doesn't count.
-- Since 9^5*7 has six digits, the numbers must all be <6 digits.
import Data.Char

digits n = map digitToInt (show n)
sumPowers n = sum (map (^5) (digits n))

main = print (sum (filter (\x -> sumPowers x == x) [2..999999]))
