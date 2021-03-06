import Data.Function ((&))
import Data.List (partition)

paths :: Eq a => a -> a -> [(a, a)] -> [[a]]
paths x y es = recur x es
  where
    recur _ [] = []
    recur u es = xs ++ (map snd us >>= flip recur vs & map (u :))
      where
        (us, vs) = partition ((==) u . fst) es
        xs = us & filter ((==) y . snd) & map (\(a, b) -> [a, b])

main :: IO ()
main = do
  print $ paths 1 4 [(1, 2), (2, 3), (1, 3), (3, 4), (4, 2), (5, 6)]
  print $ paths 2 6 [(1, 2), (2, 3), (1, 3), (3, 4), (4, 2), (5, 6)]
