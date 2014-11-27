combinations :: Int -> [a] -> [[a]]
combinations n ys@(x:xs) 
  | n == 1 = map (flip (:) []) ys
  | n > length ys = []
  | otherwise = zipWith (:) (repeat x) (combinations (n-1) xs) ++ combinations n xs