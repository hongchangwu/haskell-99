import           Data.List  (elemIndex)
import           Data.Maybe (fromJust)
import           Tree

treeToPreorder :: Tree Char -> String
treeToPreorder Empty          = ""
treeToPreorder (Branch x l r) = [x] ++ treeToPreorder l ++ treeToPreorder r

treeToInorder :: Tree Char -> String
treeToInorder Empty          = ""
treeToInorder (Branch x l r) = treeToInorder l ++ [x] ++ treeToInorder r

preInTree :: String -> String -> Tree Char
preInTree ""     "" = Empty
preInTree (x:xs) ys = Branch x (preInTree xs1 ys1) (preInTree xs2 ys2)
  where
    i = fromJust $ elemIndex x ys
    (xs1, xs2) = splitAt i xs
    ys1 = take i ys
    ys2 = drop (i + 1) ys

main :: IO ()
main = do
  let t = stringToTree "a(b(d,e),c(,f(g,)))"
      po = treeToPreorder t
      io = treeToInorder t
  print $ preInTree po io
