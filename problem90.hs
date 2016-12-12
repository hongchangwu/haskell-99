import Control.Monad (guard)

queens :: Int -> [[Int]]
queens n = queens' [([], [])] 0
  where
    ks = take n [0 ..]
    queens' yss j
      | j >= n = map (map (+ 1) . reverse . fst) yss
      | otherwise =
        let yss' = do
              (xs, ijs) <- yss
              i <- ks
              guard $ valid i j ijs
              return (i : xs, rows i ++ cols j ++ diags i j ++ ijs)
        in queens' yss' (succ j)
    valid i j ijs = (i, j) `notElem` ijs
    rows i = [(i, k) | k <- ks]
    cols j = [(k, j) | k <- ks]
    diags i j = [(i + k, j + k) | k <- [max (-i) (-j)..min (n - i) (n - j)]] ++
                [(i - k, j + k) | k <- [max (i - n) (-j)..min i (n - j)]]

main :: IO ()
main = do
  print $ length (queens 8)
  print $ head (queens 8)
