import Tree

countLeaves :: Tree a -> Int
countLeaves (Branch _ Empty Empty) = 1
countLeaves Empty = 0
countLeaves (Branch _ u v) = countLeaves u + countLeaves v