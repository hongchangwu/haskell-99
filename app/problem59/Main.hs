import           Tree

hbalTree :: a -> Int -> [Tree a]
hbalTree a 0 = [Empty]
hbalTree a 1 = [leaf a]
hbalTree a h =
  [Branch a u v | u <- hbalTree a (h - 2), v <- hbalTree a (h - 1)] ++
  [Branch a u v | u <- hbalTree a (h - 1), v <- hbalTree a (h - 1)] ++
  [Branch a u v | u <- hbalTree a (h - 1), v <- hbalTree a (h - 2)]

main :: IO ()
main = print . take 4 $ hbalTree 'x' 3
