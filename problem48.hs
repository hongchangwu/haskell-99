import Control.Monad (replicateM)
import Logic
import Text.Printf

tablen :: Int -> ([Bool] -> Bool) -> IO ()
tablen n f = mapM_ printRow (replicateM n [True, False])
  where printRow xs = do 
          mapM_ (\x -> putStr $ printf "%-5s " (show x)) xs
          printf "%-5s\n" (show $ f xs)