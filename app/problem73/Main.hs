import           Data.List (intersperse)
import           MultiTree

displayLisp :: MultiTree Char -> String
displayLisp (Node x []) = [x]
displayLisp (Node x ss) =
  "(" ++ [x] ++ " " ++ (concat . intersperse " ") (map displayLisp ss) ++ ")"

main :: IO ()
main = do
  print $ displayLisp tree1
  print $ displayLisp tree2
  print $ displayLisp tree3
  print $ displayLisp tree4
  print $ displayLisp tree5
