import Tree

tree65 =
  Branch
    'n'
    ( Branch
        'k'
        ( Branch
            'c'
            (Branch 'a' Empty Empty)
            ( Branch
                'e'
                (Branch 'd' Empty Empty)
                (Branch 'g' Empty Empty)
            )
        )
        (Branch 'm' Empty Empty)
    )
    ( Branch
        'u'
        ( Branch
            'p'
            Empty
            (Branch 'q' Empty Empty)
        )
        Empty
    )

leftSpine :: Tree (a, (Int, Int)) -> [Int]
leftSpine Empty = []
leftSpine (Branch (_, (x, _)) Empty r) = x : leftSpine r
leftSpine (Branch (_, (x, _)) l _) = x : leftSpine l

rightSpine :: Tree (a, (Int, Int)) -> [Int]
rightSpine Empty = []
rightSpine (Branch (_, (x, _)) l Empty) = x : rightSpine l
rightSpine (Branch (_, (x, _)) _ r) = x : rightSpine r

layout :: Tree a -> Tree (a, (Int, Int))
layout tree = recur tree 1
  where
    recur Empty _ = Empty
    recur (Branch a l r) h = Branch (a, (xa, h)) l' r''
      where
        l' = recur l (h + 1)
        r' = recur r (h + 1)
        d = foldr f 1 (rightSpine l' `zip` leftSpine r')
          where
            f (x, y) z = z `max` ((x - y) `div` 2 + 1)
        xa =
          case l' of
            Empty -> 1
            Branch (_, (x, _)) _ _ -> x + d
        r'' =
          case r' of
            Empty -> Empty
            Branch (_, (x, _)) _ _ ->
              mapTree (\(a, (x', y)) -> (a, (x' + xa + d - x, y))) r'

main :: IO ()
main = print $ layout tree65
