import Control.Applicative (liftA2)
import Data.List ((\\))
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
    f (x, xs) (ns, es) = (x : ns, (map ((,) x) . filter (`elem` ns)) xs ++ es)

graphToFri :: (Eq a) => Graph a -> Friendly a
graphToFri (Graph ns es) = Edge $ concatMap f ns
  where
    f n = case filter (liftA2 (||) ((==) n . fst) ((==) n . snd)) es of
      [] -> [(n, n)]
      xs -> filter ((== n) . fst) xs

friToGraph :: (Eq a) => Friendly a -> Graph a
friToGraph (Edge xs) = Graph ns es
  where
    (ns, es) = loop [] [] xs
      where
        loop ns es [] = (ns, es)
        loop ns es ((a, b) : ys) = let ns' = ns ++ if a == b then [a] else [a, b] \\ ns
                                       es' = es ++ if a == b then [] else [(a, b)]
                                       ys' = filter (\(x, y) -> (x /= a || y /= b) && (x /= b || y /= a)) ys
                                   in loop ns' es' ys'
    
adjToFri :: (Eq a) => Adjacency a -> Friendly a
adjToFri = graphToFri . adjToGraph

friToAdj :: (Eq a) => Friendly a -> Adjacency a
friToAdj = graphToAdj . friToGraph
