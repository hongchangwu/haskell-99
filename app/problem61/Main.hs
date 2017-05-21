import           Tree

countLeaves :: Tree a -> Int
countLeaves Empty                  = 0
countLeaves (Branch _ Empty Empty) = 1
countLeaves (Branch _ u v)         = countLeaves u + countLeaves v

main :: IO ()
main = print $ countLeaves tree4
