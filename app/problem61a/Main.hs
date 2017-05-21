import           Tree

leaves :: Tree a -> [a]
leaves Empty                  = []
leaves (Branch x Empty Empty) = [x]
leaves (Branch x u v)         = leaves u ++ leaves v

main :: IO ()
main = print $ leaves tree4
