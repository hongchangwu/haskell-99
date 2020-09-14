import Tree

cbalTree :: Int -> [Tree Char]
cbalTree n = filter isBalanced (trees 'x' n)

main :: IO ()
main = print $ cbalTree 4
