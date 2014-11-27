removeAt :: Int -> [x] -> [x]
removeAt n x = take (n-1) x ++ drop n x
