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

isCompleteBinaryTree :: Tree Char -> Bool
isCompleteBinaryTree t = t == completeBinaryTree n
  where
    n = count t

main :: IO ()
main = do
  print $ completeBinaryTree 4
  print . isCompleteBinaryTree $
    Branch 'x' (Branch 'x' Empty Empty) (Branch 'x' Empty Empty)
