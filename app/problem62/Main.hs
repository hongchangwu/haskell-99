import Tree

internals :: Tree a -> [a]
internals Empty = []
internals (Branch x Empty Empty) = []
internals (Branch x u v) = x : internals u ++ internals v

main :: IO ()
main = print $ internals tree4
