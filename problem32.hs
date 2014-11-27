myGCD :: Integral a => a -> a -> a
myGCD x y = abs $ euclid x' y'
  where euclid x y
          | r == 0    = y
          | otherwise = euclid y r
          where r = rem x y
        x' = max x y
        y' = min x y

