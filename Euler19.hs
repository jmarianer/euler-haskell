-- Between Jan 1901 and Dec 2000 (inclusive), how many months began on a Sunday?

-- I'm deliberately not using any date libraries here. Dates are triplets of (month, day, year)
isLeapYear y = (y `mod` 4 == 0) && (y `mod` 100 /= 0) || (y `mod` 400 == 0)
daysInMonth m y =
  if (isLeapYear y && m == 2)
  then 29
  else [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31] !! (m-1)

nextMonth m y =
  if m == 12
  then (1, 1, y+1)
  else (m+1, 1, y)
nextDay (m, d, y) =
  if (daysInMonth m y == d)
  then nextMonth m y
  else (m, d+1, y)

firstDay = (1, 1, 1900)
lastDay = (12, 31, 2000)
days' = firstDay : map nextDay days'
days = takeWhile (/= nextDay lastDay) days'

daysOfWeek = map (\i -> i `mod` 7) [1..]  -- Start on 1=Monday. Check for 0=Sunday

main = print (length (filter (\((m, d, y), dow) -> dow == 0 && d == 1 && y > 1900) (zip days daysOfWeek)))
