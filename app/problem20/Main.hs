removeAt :: Int -> [a] -> [a]
removeAt n x = take (n - 1) x ++ drop n x

main :: IO ()
main = print $ removeAt 2 "abcd"
