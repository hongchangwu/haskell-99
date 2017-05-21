import           Tree

completeBinaryTree :: Int -> Tree Char
completeBinaryTree 0 = Empty
completeBinaryTree n = numberedTree 1
  where
    numberedTree x
      | (2 * x + 1 <= n) =
        Branch 'x' (numberedTree (2 * x)) (numberedTree (2 * x + 1))
      | (2 * x <= n) = Branch 'x' (numberedTree (2 * x)) Empty
      | otherwise = Branch 'x' Empty Empty

main :: IO ()
main = do
  print $ completeBinaryTree 4
