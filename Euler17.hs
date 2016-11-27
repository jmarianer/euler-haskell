-- Count the letters (not including spaces or punctuation) in the text "one, two, three, ..., one thousand"
import Data.Char

text 1 = "one"
text 2 = "two"
text 3 = "three"
text 4 = "four"
text 5 = "five"
text 6 = "six"
text 7 = "seven"
text 8 = "eight"
text 9 = "nine"
text 10 = "ten"
text 11 = "eleven"
text 12 = "twelve"
text 13 = "thirteen"
text 14 = "fourteen"
text 15 = "fifteen"
text 16 = "sixteen"
text 17 = "seventeen"
text 18 = "eighteen"
text 19 = "nineteen"
text 20 = "twenty"
text 30 = "thirty"
text 40 = "forty"
text 50 = "fifty"
text 60 = "sixty"
text 70 = "seventy"
text 80 = "eighty"
text 90 = "ninety"

-- Hard-code this one because we don't care about numbers over 1000
text 1000 = "one thousand"

text n =
  if (n < 100)
  then (text (n `div` 10 * 10) ++ "-" ++ text (n `mod` 10))
  else (text (n `div` 100)) ++ " hundred" ++ lowerDigits (n `mod` 100)
  where lowerDigits 0 = ""
        lowerDigits n = " and " ++ text n

countLetters n = length (filter isAlpha (text n))

main = print (sum (map countLetters [1..1000]))
