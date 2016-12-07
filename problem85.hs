import Data.Function ((&))
import Data.List (find, permutations, sort)
import Data.Map (Map)
import Data.Maybe (fromJust)
import Graph
import qualified Data.Map as Map

iso :: (Eq a, Ord a) => Graph a -> Graph a -> Bool
iso g1@(Graph ns1 _) g2@(Graph ns2 _)
  | length ns1 /= length ns2 =
      False
  | otherwise =
      permutations ns2 &
      map (zip ns1) &
      any (\xys ->
             let f x = lookup x xys
             in all
                (\n -> sort (map f (lookup n adj1)) == sort (lookup (f n) adj2))
                ns1)
  where
    Adj adj1 = graphToAdj g1
    Adj adj2 = graphToAdj g2
    lookup x = snd . fromJust . find ((== x) . fst)

graphG1 :: Graph Int
graphG1 = Graph [1,2,3,4,5,6,7,8] [(1,5),(1,6),(1,7),(2,5),(2,6),(2,8),(3,5),(3,7),(3,8),(4,6),(4,7),(4,8)]

graphH1 :: Graph Int
graphH1 = Graph [1,2,3,4,5,6,7,8] [(1,2),(1,4),(1,5),(6,2),(6,5),(6,7),(8,4),(8,5),(8,7),(3,2),(3,4),(3,7)]

main :: IO ()
main = print $ iso graphG1 graphH1
