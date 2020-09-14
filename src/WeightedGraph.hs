module WeightedGraph where

data WeightedGraph a b
  = WeightedGraph [a] [(a, a, b)]
  deriving (Eq, Show)
