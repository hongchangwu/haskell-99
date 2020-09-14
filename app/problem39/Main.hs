import Number (isPrime, primesTME)

primesR :: Integral a => a -> a -> [a]
primesR l r
  | r < 100 || l * l < r =
    case odd l of
      True -> [x | x <- [l, l + 2 .. r], isPrime x]
      _ -> [x | x <- [l + 1, l + 3 .. r], isPrime x]
  | otherwise = takeWhile (<= r) . dropWhile (< l) $ primesTME

main :: IO ()
main = print $ primesR 10 20
