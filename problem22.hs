range :: Int -> Int -> [Int]
range x y
  | y >= x    = take (y-x+1) $ iterate (+ 1) x
  | otherwise = take (x-y+1) $ iterate (flip (-) 1) x