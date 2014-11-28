import Text.Regex (mkRegex, subRegex)
import Tree

splitOne :: Char -> String -> (String, String)
splitOne c = go ""
  where go xs "" = (reverse xs, "")
        go xs (y:ys)
          | y == c = (reverse xs, ys)
          | otherwise = go (y:xs) ys

stringToTree :: String -> Tree Char
stringToTree []     = Empty
stringToTree (x:xs) = let (ls, rs) = splitOne ',' xs
                          strip s = subRegex (mkRegex "[()]") s ""
                      in Branch x (stringToTree (strip ls)) (stringToTree (strip rs))

treeToString :: Tree Char -> String
treeToString Empty          = ""
treeToString (Branch x l r) = let ls = treeToString l
                                  rs = treeToString r
                              in if null ls && null rs
                                 then [x]
                                 else [x] ++ "(" ++ ls ++ "," ++ rs ++ ")"
