rotate :: [a] -> Int -> [a]
rotate x n = (drop n' x) ++ (take n' x)
  where
    n' = n `mod` length x

main :: IO ()
main = do
  print $ rotate ['a', 'b', 'c', 'd', 'e', 'f', 'g', 'h'] 3
  print $ rotate ['a', 'b', 'c', 'd', 'e', 'f', 'g', 'h'] (-2)
