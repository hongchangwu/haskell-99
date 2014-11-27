split :: [x] -> Int -> ([x],[x])
split x n = (take n x, drop n x)
