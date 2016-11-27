-- Find the sum of the digits in the number 100!

-- Another easy one thanks to bignums.

digs 0 = []
digs x = x `mod` 10 : digs (x `div` 10)

main = print (sum (digs (product [1..100])))
