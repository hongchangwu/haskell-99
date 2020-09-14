import Tree

tree64 :: Tree Char
tree64 =
  Branch
    'n'
    ( Branch
        'k'
        ( Branch
            'c'
            (Branch 'a' Empty Empty)
            ( Branch
                'h'
                ( Branch
                    'g'
                    (Branch 'e' Empty Empty)
                    Empty
                )
                Empty
            )
        )
        (Branch 'm' Empty Empty)
    )
    ( Branch
        'u'
        ( Branch
            'p'
            Empty
            ( Branch
                's'
                (Branch 'q' Empty Empty)
                Empty
            )
        )
        Empty
    )

layout :: Tree a -> Tree (a, (Int, Int))
layout tree = fst $ recur tree 1
  where
    recur Empty _ = (Empty, 0)
    recur (Branch a l r) h =
      let (l', nl) = recur l (h + 1)
          (r', nr) = recur r (h + 1)
          r'' = mapTree (\(a, (x, y)) -> (a, (x + nl + 1, y))) r'
       in (Branch (a, (nl + 1, h)) l' r'', nl + nr + 1)

main :: IO ()
main = print $ layout tree64
