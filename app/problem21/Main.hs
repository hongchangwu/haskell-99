insertAt :: a -> [a] -> Int -> [a]
insertAt x xs i
  | i < 1 || i > length xs + 1 = error "Index error"
  | otherwise = take (i - 1) xs ++ (x : drop (i - 1) xs)

main :: IO ()
main = print $ insertAt 'X' "abcd" 2
