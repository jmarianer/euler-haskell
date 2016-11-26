-- Find the product of a pythagorean triplet whose sum is 1000

ints = 1:map (+1) ints

-- Partition 1000 into three numbers
partitions = [(x, y, 1000-x-y) | x <- (take 998 ints), y <- (drop x (take (1000-x-1) ints))]

isPythagoreanTriple (x, y, z) = x^2 + y^2 == z^2
tripletProduct (x, y, z) = x * y * z

main = print (tripletProduct (head (filter isPythagoreanTriple partitions)))
