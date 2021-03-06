import MultiTree

stringToTree :: String -> MultiTree Char
stringToTree xs = Node x (map stringToTree ss)
  where
    (x, ss) = tokenize xs
    tokenize s = (head s, third $ foldr build (0, [], []) (init s))
      where
        build :: Char -> (Int, String, [String]) -> (Int, String, [String])
        build '^' (n, cs, ss) = (n + 1, '^' : cs, ss)
        build c (n, cs, ss) =
          let cs' = c : cs
           in if n == 1
                then (0, [], cs' : ss)
                else (n - 1, cs', ss)
        third (_, _, x) = x

treeToString :: MultiTree Char -> String
treeToString (Node x ts) = x : concatMap treeToString ts ++ "^"

main :: IO ()
main = do
  print $ stringToTree "afg^^c^bd^e^^^"
  print $
    treeToString
      ( Node
          'a'
          [ Node 'f' [Node 'g' []],
            Node 'c' [],
            Node 'b' [Node 'd' [], Node 'e' []]
          ]
      )
