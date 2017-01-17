-- How many different ways can £2 be made using any number of coins?
coins = [1,2,5,10,20,50,100,200]

makeSum :: [Int] -> Int -> [[Int]]
makeSum [] 0 = [[]]
makeSum [] n = []
makeSum (c:cs) n = with ++ without
  where without = makeSum cs n
        with = if (n < c)
               then []
               else map (c:) (makeSum (c:cs) (n-c))

main = print (length (makeSum coins 200))
