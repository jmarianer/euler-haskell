digits n
  | n < 10    = [n]
  | otherwise = digits (n `div` 10) ++ [n `mod` 10]

fracPart = concat . map digits $ [1..]

main = print . product . map (fracPart !!) $ [0, 9, 99, 999, 9999, 99999, 999999]
