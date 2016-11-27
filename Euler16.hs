-- Find the sum of the digits of 2^1000 in decimal

-- This one's a gimme because Haskell has native (fast) Bignums.

digs 0 = []
digs x = x `mod` 10 : digs (x `div` 10)

main = print (sum (digs (2^1000)))
