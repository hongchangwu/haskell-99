import MultiTree

bottomUp :: MultiTree Char -> String
bottomUp (Node x ss) = concatMap bottomUp ss ++ [x]

main :: IO ()
main = print $ bottomUp tree5
