import Graph

connectedcomponents :: Eq a => Graph a -> [[a]]
connectedcomponents g@(Graph ns _) = iter ([], []) ns
  where
    iter (xss, _) [] = reverse xss
    iter (xss, ys) (n : ns') =
      if n `elem` ys
      then iter (xss, ys) ns'
      else let ys' = reverse $ dfs [] n
           in iter (ys' : xss, ys' ++ ys) ns'
    dfs acc n = if n `elem` acc
                then acc
                else foldl dfs (n : acc) (neighbors n g)

main :: IO ()
main = do
  let g = Graph [1,2,3,4,5,6,7] [(1,2),(2,3),(1,4),(3,4),(5,2),(5,4),(6,7)] :: Graph Int
  print $ connectedcomponents g
