import Data.List (foldl', find, sortBy)
import Data.Maybe (fromJust, mapMaybe)
import Data.Ord (comparing)
import Graph

degree :: (Eq a) => Graph a -> a -> Int
degree g@(Graph _ es) n = foldl' f 0 es
  where
    f z (a, b)
      | a == n || b == n = succ z
      | otherwise = z

sortByDegree :: (Eq a) => Graph a -> [a]
sortByDegree g@(Graph ns _) = sortBy (comparing (degree g)) ns

kcolor :: (Eq a) => Graph a -> [(a, Int)]
kcolor g  = foldr f [] . sortByDegree $ g
  where
    Adj adjs = graphToAdj g
    f n cs = (n, choose 1) : cs
      where
        neighbors = snd . fromJust . find ((==) n . fst) $ adjs
        h (n, k) = if n `elem` neighbors then Just k else Nothing
        choose k
          | k `notElem` mapMaybe h cs = k
          | otherwise = choose (succ k)
