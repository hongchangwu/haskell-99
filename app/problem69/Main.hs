import           Tree

tree2ds :: Tree Char -> String
tree2ds Empty          = "."
tree2ds (Branch x l r) = [x] ++ tree2ds l ++ tree2ds r

ds2tree :: String -> Tree Char
ds2tree s =
  let (t, rs) = extract s
  in if not (null rs)
       then error ("invalid synatx: " ++ s)
       else t
  where
    extract :: String -> (Tree Char, String)
    extract [] = error "invalid syntax: empty"
    extract ('.':xs) = (Empty, xs)
    extract (x:xs) =
      let (lt, us) = extract xs
          (rt, vs) = extract us
      in (Branch x lt rt, vs)

main :: IO ()
main = do
  let example = "abd..e..c.fg..."
  print $ ds2tree example
  print $
    tree2ds
      (Branch
         'x'
         (Branch 'y' Empty Empty)
         (Branch 'z' (Branch '0' Empty Empty) Empty))
