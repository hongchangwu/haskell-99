import Data.Function (on)
import Data.List (groupBy, sortBy)

lsort :: [[a]] -> [[a]]
lsort = sortBy (\x y -> compare (length x) (length y))

lfsort :: [[a]] -> [[a]]
lfsort = concat . lsort . groupBy ((==) `on` length) . lsort

main :: IO ()
main = do
  print $ lsort ["abc", "de", "fgh", "de", "ijkl", "mn", "o"]
  print $ lfsort ["abc", "de", "fgh", "de", "ijkl", "mn", "o"]
