data SingleOrMultiple a
  = Multiple Int a
  | Single a
  deriving (Show)

encodeModified :: Eq a => [a] -> [SingleOrMultiple a]
encodeModified [] = []
encodeModified [x] = [Single x]
encodeModified (x : xs) =
  let (same, other) = span (== x) xs
      first
        | length same == 0 = Single x
        | otherwise = Multiple (1 + length same) x
   in (first : encodeModified other)

main :: IO ()
main = print $ encodeModified "aaaabccaadeeee"
