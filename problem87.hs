import Data.List (find)
import Data.Maybe (fromJust)
import Graph

depthfirst :: (Eq a) => Graph a -> a -> [a]
depthfirst g@(Graph ns es) = reverse . visit []
  where
    Adj adjs = graphToAdj g
    neighbors n  = snd . fromJust . find ((==) n . fst) $ adjs
    visit acc n = if n `elem` acc
                  then acc
                  else foldl visit (n : acc) (neighbors n)
