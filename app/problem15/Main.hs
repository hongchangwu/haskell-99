repli :: [a] -> Int -> [a]
repli x n = concatMap (replicate n) x

main :: IO ()
main = print $ repli "abc" 3
