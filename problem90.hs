import Control.Monad (foldM)
import Data.List

queens :: Int -> [[Int]]
queens n = map reverse $ foldM next [] [1..n]
  where
    next xs _ = [(i : xs) | i <- [1..n] \\ xs, safe i xs]
    safe i xs = and [i /= r + n && i /= r - n | (n, r) <- zip [1..] xs]

main :: IO ()
main = do
  print $ length (queens 8)
  print $ head (queens 8)
