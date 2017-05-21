import           Data.List (find, group)

primeFactors :: Integral a => a -> [a]
primeFactors n =
  case m of
    Just x  -> x : primeFactors (quot n x)
    Nothing -> [n]
  where
    m = find ((== 0) . (rem n)) [x | x <- [2 .. n], x * x <= n]

primeFactorsMult = map (\xs -> (head xs, length xs)) . group . primeFactors

totient = foldr (\(p, m) acc -> (p-1) * p ^ (m-1) * acc) 1 . primeFactorsMult

main :: IO ()
main = print $ totient 10090
