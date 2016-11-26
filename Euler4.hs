-- Find the largest palindrome that is a product of two three-digit numbers

ints = 1:map (+1) ints
thru999 = take 999 ints

-- Credit: http://stackoverflow.com/questions/3963269/split-a-number-into-its-digits-with-haskell
digs 0 = []
digs x = x `mod` 10 : digs (x `div` 10)

isPalindrome x = digs x == reverse (digs x)
cartProd xs ys = [x * y | x <- xs, y <- ys]
maxList xs = foldr max 0 xs
main = print (maxList (filter isPalindrome (cartProd thru999 thru999)))
