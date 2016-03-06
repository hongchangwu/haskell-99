import Data.List (partition)

cycle :: Eq a => a ->  [(a, a)] -> [[a]]
cycle x = recur x
  where
    recur u [] = []
    recur u es = xs ++ (map snd us >>= map (u :) . flip recur vs)
      where
        (us, vs) = partition ((==) u . fst) es
        xs = map (\(a, b) -> [a, b]) . filter ((== x) . snd) $ us

    
