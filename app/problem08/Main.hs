compress :: Eq a => [a] -> [a]
compress [] = []
compress (x : xs) = x : compress (dropWhile (== x) xs)

main :: IO ()
main = print $ compress "aaaabccaadeeee"
