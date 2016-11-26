-- Print the largest prime factor of 600851475143
ints = 1:map (+1) ints
mult k n = (n `mod` k) == 0

factors n = filter (\k -> (n `mod` k) == 0) (takeWhile (<=n) (tail ints))

{- This is too slow...
 - main = print (filter prime (factors 600851475143))
 -}

smallestFactor n = head (factors n)
largestPrimeFactor n = if k == n then n else largestPrimeFactor (n `quot` k)
  where k = smallestFactor n
main = print (largestPrimeFactor 600851475143)
