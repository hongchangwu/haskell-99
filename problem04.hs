myLength :: [x] -> Int
myLength []     = 0
myLength (_:xs) = 1 + myLength xs
