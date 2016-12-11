import Data.List (find)
import Graph

bipartite :: Eq a => Graph a -> Bool
bipartite g@(Graph ns _) = iter [] ns
  where
    iter _ [] = True
    iter xs (n:ns') =
      if n `elem` xs
        then iter xs ns'
        else maybe False (\xs' -> iter (xs' ++ xs) ns') (dfs n)
    dfs = dfs' ([], True)
      where
        dfs' (ys, c) n =
          case get n ys of
            Just c' ->
              -- Odd cycle found, graph not bipartite
              if c' /= c
                then Nothing
                else Just (fmap fst ys)
            Nothing ->
              foldr1 merge . map (dfs' ((n, c) : ys, not c)) $ neighbors n g
          where
            get x = fmap snd . find ((==) x . fst)
            merge Nothing _ = Nothing
            merge _ Nothing = Nothing
            merge (Just ys1) (Just ys2) = Just (ys1 ++ ys2)

main :: IO ()
main = do
  let g1 = Graph [1,2,3,4,5] [(1,2),(2,3),(1,4),(3,4),(5,2),(5,4)] :: Graph Int
      g2 = Graph [1,2,3,4,5] [(1,2),(2,3),(1,4),(3,4),(5,2),(5,4),(1,3)] :: Graph Int
  print $ bipartite g1
  print $ bipartite g2
