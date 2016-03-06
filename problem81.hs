import Data.List (partition)

paths :: Eq a => a -> a -> [(a, a)] -> [[a]]
paths x y es = recur x es
  where
    recur _ [] = []
    recur u es = xs ++ (map snd us >>= map (u :) . flip recur vs)
      where
        (us, vs) = partition ((==) u . fst) es
        xs = map (\(a, b) -> [a, b]) . filter ((== y) . snd) $ us
