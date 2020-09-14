module MultiTree where

data MultiTree a
  = Node a [MultiTree a]
  deriving (Eq, Show)

tree1 :: MultiTree Char
tree1 = Node 'a' []

tree2 :: MultiTree Char
tree2 = Node 'a' [Node 'b' []]

tree3 :: MultiTree Char
tree3 = Node 'a' [Node 'b' [Node 'c' []]]

tree4 :: MultiTree Char
tree4 = Node 'b' [Node 'd' [], Node 'e' []]

tree5 :: MultiTree Char
tree5 =
  Node
    'a'
    [Node 'f' [Node 'g' []], Node 'c' [], Node 'b' [Node 'd' [], Node 'e' []]]
