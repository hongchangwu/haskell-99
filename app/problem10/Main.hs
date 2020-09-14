encode :: Eq a => [a] -> [(Int, a)]
encode [] = []
encode (x : xs) =
  let (same, other) = span (== x) xs
   in (1 + length same, x) : encode other

main :: IO ()
main = print $ encode "aaaabccaadeeee"
