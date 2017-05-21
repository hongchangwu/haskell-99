dropEvery :: [a] -> Int -> [a]
dropEvery [] _ = []
dropEvery x n  = take (n - 1) x ++ dropEvery (drop n x) n

main :: IO ()
main = print $ dropEvery "abcdefghik" 3
