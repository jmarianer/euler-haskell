-- Find the number of routes from (0, 0) to (20, 20) only moving right or down.

-- Equivalently, find the number of routes from (i, j) to (0, 0) only moving up or left.

{- Slow due to recursion
 - routeCount 0 0 = 1
 - routeCount 0 j = routeCount 0 (j-1)
 - routeCount i 0 = routeCount (i-1) 0
 - routeCount i j = routeCount (i-1) j + routeCount i (j-1)
 -}

-- Memoized: https://wiki.haskell.org/Memoization
routeCount i j = routeCounts !! i !! j
routeCounts = map (\i -> map (\j -> rc i j) [0..20]) [0..20]
  where rc 0 0 = 1
        rc 0 j = routeCount 0 (j-1)
        rc i 0 = routeCount (i-1) 0
        rc i j = routeCount (i-1) j + routeCount i (j-1)


main = print (routeCount 20 20)
