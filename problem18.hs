slice :: [x] -> Int -> Int -> [x]
slice x i j = take (j-i+1) $ drop (i-1) x
