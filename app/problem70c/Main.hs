import           MultiTree

nnodes :: MultiTree a -> Int
nnodes (Node _ ts) = 1 + sum (map nnodes ts)

main :: IO ()
main = print $ nnodes tree2
