myReverse :: [x] -> [x]
myReverse = foldl (flip (:)) []
