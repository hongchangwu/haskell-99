import Data.List (find)

primeFactors :: Integral a => a -> [a]
primeFactors n =
  case m of
    Just x -> x : primeFactors (quot n x)
    Nothing -> [n]
  where
    m = find ((== 0) . (rem n)) [x | x <- [2 .. n], x * x <= n]

main :: IO ()
main = print $ primeFactors 315
