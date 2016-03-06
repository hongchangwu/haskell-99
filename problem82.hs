import Data.Function ((&))
import Data.List (partition)

cycle :: Eq a => a ->  [(a, a)] -> [[a]]
cycle x = recur x
  where
    recur u [] = []
    recur u es = xs ++ (map snd us >>= flip recur vs & map (u :))
      where
        (us, vs) = partition ((==) u . fst) es
        xs = us & filter ((== x) . snd) & map (\(a, b) -> [a, b])
