import           Data.List (find)

primeFactorsMult :: Integral a => a -> [(a, Int)]
primeFactorsMult n =
  case m of
    Just x ->
      case rem q x of
        0 ->
          map
            (\(a, b) ->
               if a == x
                 then (a, b + 1)
                 else (a, b)) $
          primeFactorsMult q
        _ -> (x, 1) : primeFactorsMult q
      where q = quot n x
    Nothing -> [(n, 1)]
  where
    m = find ((== 0) . (rem n)) [x | x <- [2 .. n], x * x <= n]

main :: IO ()
main = print $ primeFactorsMult 315
