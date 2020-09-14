import Tree

main :: IO ()
main = do
  print $ symmetric (Branch 'x' (Branch 'x' Empty Empty) Empty)
  print $ symmetric (Branch 'x' (Branch 'x' Empty Empty) (Branch 'x' Empty Empty))
