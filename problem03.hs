elementAt :: [x] -> Int -> x
elementAt [] _     = error "empty list"
elementAt (x:xs) 1 = x
elementAt (x:xs) k
  | k < 1     = error "negative index"
  | otherwise = elementAt xs (k-1)
