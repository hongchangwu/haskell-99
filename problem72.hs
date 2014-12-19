import MultiTree

bottom_up :: MultiTree Char -> String
bottom_up (Node x ss) = concatMap bottom_up ss ++ [x]
