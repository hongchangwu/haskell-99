split :: [a] -> Int -> ([a], [a])
split x n = (take n x, drop n x)

main :: IO ()
main = print $ split "abcdefghik" 3
