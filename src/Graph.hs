module Graph where

import Control.Applicative (liftA2)
import Data.Function ((&))
import Data.List ((\\), find, permutations, sort)
import Data.Maybe (fromJust, mapMaybe)

data Graph a
  = Graph [a] [(a, a)]
  deriving (Eq, Show)

newtype Adjacency a
  = Adj [(a, [a])]
  deriving (Eq, Show)

newtype Friendly a
  = Edge [(a, a)]
  deriving (Eq, Show)

graphToAdj :: (Eq a) => Graph a -> Adjacency a
graphToAdj (Graph [] []) = Adj []
graphToAdj (Graph ns es) = Adj $ adjs ns es
  where
    adjs [] _ = []
    adjs (x : xs) es' = (x, mapMaybe f es') : adjs xs es'
      where
        f (a, b)
          | a == x = Just b
          | b == x = Just a
          | otherwise = Nothing

adjToGraph :: (Eq a) => Adjacency a -> Graph a
adjToGraph (Adj adjs) = Graph ns es
  where
    (ns, es) = foldr f ([], []) adjs
    f (x, xs) (ns', es') = (x : ns', (map ((,) x) . filter (`elem` ns')) xs ++ es')

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
        loop ns' es' [] = (ns', es')
        loop ns' es' ((a, b) : ys) =
          let ns'' = ns' ++ if a == b then [a] else [a, b] \\ ns'
              es'' = es' ++ if a == b then [] else [(a, b)]
              ys' = filter (\(x, y) -> (x /= a || y /= b) && (x /= b || y /= a)) ys
           in loop ns'' es'' ys'

adjToFri :: (Eq a) => Adjacency a -> Friendly a
adjToFri = graphToFri . adjToGraph

friToAdj :: (Eq a) => Friendly a -> Adjacency a
friToAdj = graphToAdj . friToGraph

neighbors :: (Eq a) => a -> Graph a -> [a]
neighbors n (Graph _ es) = mapMaybe f es
  where
    f (a, b)
      | a == n = Just b
      | b == n = Just a
      | otherwise = Nothing

iso :: (Eq a, Ord a) => Graph a -> Graph a -> Bool
iso g1@(Graph ns1 _) g2@(Graph ns2 _)
  | length ns1 /= length ns2 = False
  | otherwise =
    permutations ns2 & map (zip ns1)
      & any
        ( \xys ->
            let f x = snd . fromJust . find ((== x) . fst) $ xys
             in all
                  (\n -> sort (map f (neighbors n g1)) == sort (neighbors (f n) g2))
                  ns1
        )
