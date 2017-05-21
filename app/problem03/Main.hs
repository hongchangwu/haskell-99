elementAt :: [a] -> Int -> a
elementAt [] _ = error "empty list"
elementAt (x:xs) 1 = x
elementAt (x:xs) k
  | k < 1     = error "negative index"
  | otherwise = elementAt xs (k-1)

main :: IO ()
main = do
  print $ elementAt [1, 2, 3] 2
  print $ elementAt "Haskell" 5
