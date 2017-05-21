import           Control.Applicative
import           Data.List           (minimumBy)
import           Data.Ord            (comparing)
import           WeightedGraph

prim :: (Eq a, Ord b) => WeightedGraph a b -> WeightedGraph a b
prim (WeightedGraph ns es) = prim' ns []
  where
    third (_, _, z) = z
    prim' [] es' = WeightedGraph ns es'
    prim' ns' es' = prim' (filter (liftA2 (&&) ((/=) u) ((/=) v)) ns') (e : es')
      where
        e@(u, v, _) = minimumBy (comparing third) $ filter f es
        f (u, v, _) =
          if null es'
            then True
            else (u `elem` ns' && not (v `elem` ns')) ||
                 (not (u `elem` ns') && v `elem` ns')

main :: IO ()
main =
  print . prim $
  WeightedGraph
    [1, 2, 3, 4, 5]
    [ (1, 2, 12)
    , (1, 3, 34)
    , (1, 5, 78)
    , (2, 4, 55)
    , (2, 5, 32)
    , (3, 4, 61)
    , (3, 5, 44)
    , (4, 5, 93)
    ]
