import           Graph

depthfirst :: Eq a => Graph a -> a -> [a]
depthfirst g = reverse . visit []
  where
    visit acc n =
      if n `elem` acc
        then acc
        else foldl visit (n : acc) (neighbors n g)

main :: IO ()
main = do
  let g =
        Graph
          [1, 2, 3, 4, 5, 6, 7]
          [(1, 2), (2, 3), (1, 4), (3, 4), (5, 2), (5, 4), (6, 7)] :: Graph Int
  print $ depthfirst g 1
