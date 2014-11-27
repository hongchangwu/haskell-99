dropEvery :: [x] -> Int -> [x]
dropEvery [] _ = []
dropEvery x n = take (n-1) x ++ dropEvery (drop n x) n
