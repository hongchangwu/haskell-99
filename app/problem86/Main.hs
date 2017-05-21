import           Data.List  (find, foldl', sortBy)
import           Data.Maybe (fromJust, mapMaybe)
import           Data.Ord   (comparing)
import           Graph

degree :: Eq a => Graph a -> a -> Int
degree g@(Graph _ es) n = foldl' f 0 es
  where
    f z (a, b)
      | a == n || b == n = succ z
      | otherwise = z

sortByDegree :: Eq a => Graph a -> [a]
sortByDegree g@(Graph ns _) = sortBy (comparing (degree g)) ns

kcolor :: Eq a => Graph a -> [(a, Int)]
kcolor g = foldr f [] . sortByDegree $ g
  where
    f n cs = (n, choose 1) : cs
      where
        h (x, k) =
          if x `elem` (neighbors n g)
            then Just k
            else Nothing
        choose k
          | k `notElem` mapMaybe h cs = k
          | otherwise = choose (succ k)

main :: IO ()
main = do
  let g =
        Graph
          ['a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j']
          [ ('a', 'b')
          , ('a', 'e')
          , ('a', 'f')
          , ('b', 'c')
          , ('b', 'g')
          , ('c', 'd')
          , ('c', 'h')
          , ('d', 'e')
          , ('d', 'i')
          , ('e', 'j')
          , ('f', 'h')
          , ('f', 'i')
          , ('g', 'i')
          , ('g', 'j')
          , ('h', 'j')
          ]
  print $ kcolor g
