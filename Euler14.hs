-- Find the longest Collatz chain under 1M

ints = 1:map (+1) ints

next n = if (n `mod` 2 == 0) then (n `div` 2) else (3 * n + 1)
sequenceLength 1 = 1
sequenceLength n = sequenceLength (next n) + 1

-- Defining this myself without looking in the back of the book.
argmax :: Ord b => (a -> b) -> [a] -> a
argmax fn (a:as) = argmax' a (fn a) fn as

argmax' :: Ord b => a -> b -> (a -> b) -> [a] -> a
argmax' argMaxSoFar maxSoFar fn [] = argMaxSoFar
argmax' argMaxSoFar maxSoFar fn (a:as) =
  if (fn a > maxSoFar)
  then (argmax' a (fn a) fn as)
  else (argmax' argMaxSoFar maxSoFar fn as)

main = print (argmax sequenceLength (take 1000000 ints))
