data SingleOrMultiple a = Multiple Int a | Single a deriving (Show)

encode' :: Eq a => [a] -> [(Int,a)]
encode' = foldr helper []
  where
    helper x [] = [(1,x)]
    helper x (y@(a,b):ys)
      | x == b    = (1+a,b):ys
      | otherwise = (1,x):y:ys

encodeDirect :: Eq a => [a] -> [SingleOrMultiple a]
encodeDirect = map helper . encode'
                   where
    helper (1,x) = Single x
    helper (n,x) = Multiple n x
