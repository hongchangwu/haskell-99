compress :: (Eq x) => [x] -> [x]
compress []     = []
compress (x:xs) = x:(compress $ dropWhile (== x) xs) 
