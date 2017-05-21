import           Tree

tree65 = Branch 'n'
                (Branch 'k'
                        (Branch 'c'
                                (Branch 'a' Empty Empty)
                                (Branch 'e'
                                        (Branch 'd' Empty Empty)
                                        (Branch 'g' Empty Empty)
                                )
                        )
                        (Branch 'm' Empty Empty)
                )
                (Branch 'u'
                        (Branch 'p'
                                Empty
                                (Branch 'q' Empty Empty)
                        )
                        Empty
                )

layout :: Tree a -> Tree (a, (Int, Int))
layout tree = recur tree 1
  where
    hh = height tree
    recur Empty _ = Empty
    recur (Branch a l r) h =
      let d = 2 ^ (hh - h - 1)
          l' = recur l (h + 1)
          r' = recur r (h + 1)
          xa =
            case l' of
              Empty                  -> 1
              Branch (_, (x, _)) _ _ -> x + d
          r'' =
            case r' of
              Empty -> Empty
              Branch (_, (x, _)) _ _ ->
                mapTree (\(a, (x', y)) -> (a, (x' + xa + d - x, y))) r'
      in Branch (a, (xa, h)) l' r''

main :: IO ()
main = print $ layout tree65
