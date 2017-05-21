dupli :: [a] -> [a]
dupli = concatMap (replicate 2)

main :: IO ()
main = print $ dupli [1, 2, 3]
