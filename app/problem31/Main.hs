isPrime :: Integral a => a -> Bool
isPrime n
  | n < 4 = n > 1
  | otherwise =
    all (\x -> rem n x /= 0) . takeWhile (\x -> x * x <= n) $ [2 .. n]

main :: IO ()
main = print $ isPrime 7
