import Data.Maybe (mapMaybe)
import Graph

graphToAdj :: (Eq a) => Graph a -> Adjacency a
graphToAdj (Graph [] []) = Adj []
graphToAdj (Graph ns es) = Adj $ adjs ns es
  where
    adjs [] _ = []
    adjs (x : xs) es = (x, mapMaybe f es) : adjs xs es
      where
        f (a, b)
          | a == x = Just b
          | b == x = Just a
          | otherwise = Nothing

adjToGraph :: (Eq a) => Adjacency a -> Graph a
adjToGraph (Adj adjs) = Graph ns es
  where
    (ns, es) = foldr f ([], []) adjs
    f (x, xs) (ns, es) = (x : ns, (map (\y -> (x, y)) . filter (`elem` ns)) xs ++ es)
