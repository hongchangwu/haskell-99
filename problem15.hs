repli :: [x] -> Int -> [x]
repli x n = concatMap (replicate n) x
