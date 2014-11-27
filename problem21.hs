insertAt :: x -> [x] -> Int -> [x]
insertAt x xs i
  | i < 1 || i > length xs + 1 = error "Index error"
  | otherwise = take (i-1) xs ++ (x:drop (i-1) xs)