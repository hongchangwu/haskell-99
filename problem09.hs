pack :: Eq x => [x] -> [[x]]
pack [] = []
pack (x:xs) = (x:(takeWhile (== x) xs)) : (pack $ dropWhile (== x) xs)
