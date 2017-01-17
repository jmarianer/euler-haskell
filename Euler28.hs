-- Find the diagonal sum in a specific 1001-squared matrix

differences = 2:2:2:2:map (+2) differences
numbers = 1:zipWith (+) numbers differences

main = print (sum (take 2001 numbers))
