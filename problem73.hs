import Data.List (intersperse)
import MultiTree

display_lisp :: MultiTree Char -> String
display_lisp (Node x []) = [x]
display_lisp (Node x ss) = "(" ++ [x] ++ " " ++ (concat . intersperse " ") (map display_lisp ss) ++ ")"
