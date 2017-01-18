-- Find the sum of all numbers <1M that are palindromes in both decimal and binary.
digits a base
  | a < base  = [a]
  | otherwise = (a `mod` base) : digits (a `div` base) base

palindrome a base = d == reverse d
  where d = digits a base

isInteresting a = palindrome a 10 && palindrome a 2

main = print . sum . filter isInteresting $ [1..1000000]
