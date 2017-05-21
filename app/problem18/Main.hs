slice :: [a] -> Int -> Int -> [a]
slice x i j = take (j - i + 1) $ drop (i - 1) x

main :: IO ()
main = print $ slice ['a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'k'] 3 7
