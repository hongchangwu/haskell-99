module Graph where

data Graph a = Graph [a] [(a, a)]
               deriving (Eq, Show)

data Adjacency a = Adj [(a, [a])]
             deriving (Eq, Show)

data Friendly a = Edge [(a, a)]
                deriving (Eq, Show)
