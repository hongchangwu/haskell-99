import Control.Monad
import Data.Function ((&))
import Data.IntMap ((!))
import Data.List
import qualified Data.IntMap as IntMap
import qualified Data.IntSet as IntSet

vonKoch :: [(Int, Int)] -> [[Int]]
vonKoch es =
  map (f . \(a, _, _) -> a) $
  foldM next (IntMap.empty, IntSet.empty, es) [n - 1,n - 2 .. 1]
  where
    ns = es & (>>= \(u, v) -> [u, v]) & nub
    n = length ns
    next (m, s, us) k = do
      i <- [1 .. n - k]
      (a, b) <- [(i, i + k), (i + k, i)]
      (u, v) <- us
      guard $
        (IntMap.lookup u m & maybe (IntSet.notMember a s) (== a)) &&
        (IntMap.lookup v m & maybe (IntSet.notMember b s) (== b))
      return
        ( m & IntMap.insert u a & IntMap.insert v b
        , s & IntSet.insert a & IntSet.insert b
        , delete (u, v) us)
    f m = map (m !) [1 .. n]

main :: IO ()
main =
  print $
  head $
  vonKoch
    [ (1, 6)
    , (2, 6)
    , (3, 6)
    , (4, 6)
    , (5, 6)
    , (5, 7)
    , (5, 8)
    , (8, 9)
    , (5, 10)
    , (10, 11)
    , (11, 12)
    , (11, 13)
    , (13, 14)
    ]
