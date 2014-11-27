import System.Random

diffSelect :: Int -> Int -> IO [Int]
diffSelect n m
  | n < 0     = error "Negavie number"
  | otherwise = do
    gen <- getStdGen
    return $ take n [x | x <- randomRs (1, m) gen]