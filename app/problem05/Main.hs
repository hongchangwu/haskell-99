myReverse :: [a] -> [a]
myReverse = foldl (flip (:)) []

main :: IO ()
main = do
  print $ myReverse "A man, a plan, a canal, panama!"
  print $ myReverse [1, 2, 3, 4]
