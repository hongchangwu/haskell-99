import Control.Monad (guard)
import Data.List (delete)

queens :: Int -> [[Int]]
queens n = queens' [([], ks)] 0
  where
    ks = take n [0 ..]
    queens' zss j
      | j >= n = map (map (+ 1) . reverse . fst) zss
      | otherwise =
        let zss' = do
              (xs, ys) <- zss
              i <- ys
              guard $ safe i xs
              return (i : xs, delete i ys)
        in queens' zss' (succ j)
      where
        safe i xs = and [i /= r + n && i /= r - n | (n, r) <- zip [1..] xs]

main :: IO ()
main = do
  print $ length (queens 8)
  print $ head (queens 8)
