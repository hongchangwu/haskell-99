import Control.Monad
import Data.List
import Data.Ord (comparing)

moves :: Int -> (Int, Int) -> [(Int, Int)]
moves n (x, y) =
  [ (x + i, y + j)
  | (i, j) <- ((,) <$> [-1, 1] <*> [-2, 2]) ++ ((,) <$> [-2, 2] <*> [-1, 1])
  , x + i > 0 && x + i <= n && y + j > 0 && y + j <= n ]

knightsTo :: Int -> (Int, Int) -> [[(Int, Int)]]
knightsTo n (x0, y0) = foldM next [(x0, y0)] [1 .. n * n - 1]
  where
    next [] _ = error "impossible"
    next us@((x, y):_) _ =
      [ (x', y') : us
      | (x', y') <- nextMoves ]
    -- Warnsdorf's rule: choose the square from which the knight
    -- will have the fewest possible moves
      where
        nextMoves =
          sortBy (comparing $ length . (\\ us) . moves n) $ moves n (x, y) \\ us

closedKnights :: Int -> [[(Int, Int)]]
closedKnights n = filter (elem (1, 1) . moves n . head) $ knightsTo n (1, 1)

main :: IO ()
main = do
  print $ head $ knightsTo 8 (1, 1)
  print $ head $ closedKnights 8
