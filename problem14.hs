dupli :: [x] -> [x]
dupli = concatMap (replicate 2)
