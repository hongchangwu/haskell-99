import           Tree

construct :: Ord a => [a] -> Tree a
construct = foldr insert' Empty

main :: IO ()
main = do
  print $ construct [3, 2, 5, 7, 1]
  print . symmetric . construct $ [5, 3, 18, 1, 4, 12, 21]
  print . symmetric . construct $ [3, 2, 5, 7, 1]
