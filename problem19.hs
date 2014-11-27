rotate :: [x] -> Int -> [x]
rotate x n = (drop n' x) ++ (take n' x)
  where n' = n `mod` length x
