import Data.List (sortBy, groupBy)
import Data.Function (on)

lsort :: [[a]] -> [[a]]
lsort = sortBy (\x y -> compare (length x) (length y))

lfsort :: [[a]] -> [[a]]
lfsort = concat . lsort . groupBy ((==) `on` length) . lsort
