import System.Random

rndPermu :: [a] -> IO [a]
rndPermu [] = return []
rndPermu (x : xs) = do
  ind <- randomRIO (0, length xs)
  rest <- rndPermu xs
  return $
    let (ys, zs) = splitAt ind rest
     in ys ++ (x : zs)

main :: IO ()
main = rndPermu "abcdef" >>= print
