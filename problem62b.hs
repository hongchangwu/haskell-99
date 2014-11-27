import Tree

atLevel :: Tree a -> Int -> [a]
atLevel Empty _ = []
atLevel (Branch x u v) 1 = [x]
atLevel (Branch _ u v) n = atLevel u (n-1) ++ atLevel v (n-1)