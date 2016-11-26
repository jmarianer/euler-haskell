-- Print the sum of even-valued Fibonacci numbers that are less than 4M

mult k n = (n `mod` k) == 0
sumpair (a,b) = a + b
fib = 1:1:(map sumpair (zip fib (tail fib)))
main = print (sum (filter (mult 2) (takeWhile (<4000000) fib)))
