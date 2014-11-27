import Tree

construct :: Ord a => [a] -> Tree a
construct = foldl (flip insert') Empty