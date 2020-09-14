{-# LANGUAGE NamedFieldPuns #-}

import Data.List (sortBy, transpose)
import Data.Map ((!), Map)
import qualified Data.Map as Map
import Data.Ord (comparing)
import Prelude hiding (words)

type Coord = (Int, Int)

type Site = [Coord]

type Grid = Map Coord Char

data Puzzle
  = Puzzle
      { words :: [String],
        sites :: [Site],
        grid :: Grid
      }
  deriving (Show)

instance Read Puzzle where
  readsPrec =
    const $
      return . flip (,) ""
        . uncurry readPuzzle
        . fmap tail
        . break null
        . lines
    where
      readPuzzle words specs = Puzzle words' sites grid
        where
          words' = sortBy (flip $ comparing length) words
          specs' =
            [ [((i, j), x) | (x, j) <- zip row [1 ..]]
              | (row, i) <- zip specs [1 ..]
            ]
          sites = horizontalSites ++ verticalSites
            where
              findSites [] = []
              findSites xs@(x : xs')
                | snd x == ' ' = findSites xs'
                | otherwise =
                  if length us > 1
                    then map fst us : findSites vs
                    else findSites vs
                where
                  (us, vs) = break ((==) ' ' . snd) xs
              horizontalSites = concatMap findSites specs'
              verticalSites = concatMap findSites (transpose specs')
          grid = Map.fromList . filter (\(_, x) -> x /= ' ') . concat $ specs'

solve :: Puzzle -> [Grid]
solve Puzzle {words, sites, grid} = go grid words
  where
    go g [] = [g]
    go g (w : ws) = concatMap (\s -> go (update s) ws) ss
      where
        ss = filter (isValid w) sites
          where
            isValid [] [] = True
            isValid _ [] = False
            isValid [] _ = False
            isValid (x : xs) (y : ys) =
              let z = g ! y
               in (z == '.' || z == x) && isValid xs ys
        update s = foldr (uncurry Map.insert) g (zip s w)

main :: IO ()
main =
  print . solve . read $
    "ALPHA\nARES\nPOPPY\n\n  .  \n  .  \n.....\n  . .\n  . .\n    .\n"
