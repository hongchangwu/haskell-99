import Control.Monad (foldM)
import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import Data.List (foldl', nubBy)
import Graph

choose :: Int -> [a] -> [[a]]
choose 0 _ = [[]]
choose _ [] = []
choose n (x : xs) = map (x :) (choose (n - 1) xs) ++ choose n xs

regular :: Int -> Int -> [Graph Int]
regular n k =
  nubBy iso . map (friToGraph . Edge) . fst . unzip $
    foldM f ([], IntMap.empty) [1 .. n]
  where
    f (us, m) x
      | d > k = []
      | otherwise =
        [ (us ++ vs, add vs)
          | vs <- vss
        ]
      where
        d = IntMap.findWithDefault 0 x m
        add vs = foldl' g m (flatten vs)
        g m x = IntMap.insertWith (+) x 1 m
        flatten = concatMap (\(x, y) -> [x, y])
        vss =
          [ map ((,) x) ys
            | ys <- choose (k - d) [succ x .. n]
          ]

main :: IO ()
main = print . length $ regular 6 3
