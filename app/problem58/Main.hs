import Tree

symCbalTrees :: Int -> [Tree Char]
symCbalTrees n = [x | x <- trees 'x' n, isBalanced x && symmetric x]

main :: IO ()
main = print $ symCbalTrees 5
