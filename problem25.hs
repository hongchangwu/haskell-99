import System.Random

rndPermu :: [x] -> IO [x]
rndPermu []     = return []
rndPermu (x:xs) = do
  ind  <- randomRIO (0, length xs)
  rest <- rndPermu xs 
  return $ let (ys, zs) = splitAt ind rest
           in ys ++ (x:zs)