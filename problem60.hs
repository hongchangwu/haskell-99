import Tree

hbalTreeNodes :: a -> Int -> [Tree a]
hbalTreeNodes x 0 = [Empty]
hbalTreeNodes x 1 = [leaf x]
hbalTreeNodes x n = [Branch x u v | i <- [lb..ub],
                     u <- hbalTreeNodes x i,
                     v <- hbalTreeNodes x (n - i - 1)]
  where a = ceiling (fromIntegral (n - 1) / 5)
        b = floor (fromIntegral (n - 1) / 5 * 4)
        (lb, ub) = (min a b, max a b)