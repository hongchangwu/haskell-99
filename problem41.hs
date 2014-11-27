import Number (isPrime)

goldbach :: Integral a => a -> (a, a)
goldbach 4 = (2, 2)
goldbach n = head .
             filter (\(x, y) -> isPrime x && isPrime y ) .
             map (\e -> (e, n - e)) $ [1, 3..n `div` 2]

goldbachList :: Integral a => a -> a -> [(a, a)]
goldbachList l r = map goldbach xs
  where xs = if even l then [l,l+2..r] else [l+1,l+3..r]

goldbachList' :: Integral a => a -> a -> a -> [(a, a)]
goldbachList' l r x = filter (\(a, b) -> a > x && b > x) $ goldbachList l r