encode :: Eq x => [x] -> [(Int,x)]
encode [] = []
encode (x:xs) =
  let (same,other) = span (== x) xs
  in  (1 + length same, x) : encode other
