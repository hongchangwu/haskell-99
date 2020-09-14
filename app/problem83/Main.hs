import Control.Applicative
import Graph

k4 :: Graph Char
k4 =
  Graph
    ['a', 'b', 'c', 'd']
    [('a', 'b'), ('b', 'c'), ('c', 'd'), ('d', 'a'), ('a', 'c'), ('b', 'd')]

spantree :: (Eq a) => Graph a -> [Graph a]
spantree (Graph ns es) = filter (liftA2 (&&) complete allconnected) trees
  where
    trees = foldr acc [Graph [] []] es
    acc e@(u, v) gs =
      [ Graph
          ( ( if u `elem` ns'
                then []
                else [u]
            )
              ++ ( if v `elem` ns'
                     then []
                     else [v]
                 )
              ++ ns'
          )
          (e : es')
        | g@(Graph ns' es') <- gs,
          not (connected g u v)
      ]
        ++ gs
    connected (Graph _ es') u v = v `elem` visit u []
      where
        visit x xs
          | x `elem` xs = xs
          | otherwise = foldr f (x : xs) es'
          where
            f (u, v) xs =
              if u == x
                then visit v xs
                else
                  if v == x
                    then visit u xs
                    else xs
    allconnected (Graph [] _) = True
    allconnected g@(Graph (n : ns) _) = all (connected g n) ns
    complete (Graph ns' _) = length ns' == length ns

main :: IO ()
main = print . length $ spantree k4
