import           Data.List (tails)

combination :: Int -> [a] -> [([a], [a])]
combination 0 xs     = [([],xs)]
combination n []     = []
combination n (x:xs) = ts ++ ds
  where
    ts = [(x : ys, zs) | (ys, zs) <- combination (n - 1) xs]
    ds = [(ys, x : zs) | (ys, zs) <- combination n xs]

group :: [Int] -> [a] -> [[[a]]]
group [] _      = [[]]
group (n:ns) xs = [g : gs | (g, rs) <- combination n xs, gs <- group ns rs]

main :: IO ()
main = do
  print . length $
    group
      [2, 3, 4]
      ["aldo", "beat", "carla", "david", "evi", "flip", "gary", "hugo", "ida"]
  print . length $
    group
      [2, 2, 5]
      ["aldo", "beat", "carla", "david", "evi", "flip", "gary", "hugo", "ida"]
