-- Find the sum of all the amicable numbers under 10000
factors n = filter (\k -> (n `mod` k) == 0) [1..n-1]
sumFactors = sum . factors

amicable n = (sumFactors (sumFactors n) == n) && (sumFactors n /= n)

main = print (sum (filter amicable [1..10000]))
