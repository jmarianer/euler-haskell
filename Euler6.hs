-- Find the difference between the sum of the squares of the first one hundred natural numbers and the square of the sum.

ints = 1:map (+1) ints
sumOfSquares = sum (map (^2) (take 100 ints))
squareOfSum = (sum (take 100 ints)) ^ 2
main = print (squareOfSum - sumOfSquares)
