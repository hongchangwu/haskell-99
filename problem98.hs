{-# LANGUAGE NamedFieldPuns #-}

import           Control.Monad    (foldM)
import           Data.Char        (chr, ord)
import           Data.List        (foldl', intersperse, sortBy, tails,
                                   transpose)
import           Data.Ord         (comparing)
import           Data.Set         (Set)
import qualified Data.Set         as Set
import           Numeric.Interval ((...))
import qualified Numeric.Interval as Interval

type Board = Set (Int, Int)

data Constraint
  = RowConstraint { row  :: Int
                 ,  nums :: [Int]}
  | ColConstraint { col  :: Int
                 ,  nums :: [Int]}

data Nonogram = Nonogram
  { board :: Board
  , rows  :: [[Int]]
  , cols  :: [[Int]]
  }

instance Show Nonogram where
  show nonogram =
    unlines $
    [ showLine i ++ showConstraint xs
    | (xs, i) <- rows nonogram `zip` [0 ..]
    ] ++
    [' ' : showConstraint xs | xs <- transpose (cols nonogram)]
    where
      n = length (cols nonogram)
      showLine i =
        '|' :
        intersperse
          '|'
          [ if Set.member (i, j) (board nonogram)
            then 'X'
            else '_'
          | j <- [0 .. n - 1]
          ] ++
        "| "
      showDigit = chr . (+ ord '0')
      showConstraint = intersperse ' ' . map showDigit

fromRowsCols :: [[Int]] -> [[Int]] -> Nonogram
fromRowsCols = Nonogram Set.empty

solve :: Nonogram -> Nonogram
solve Nonogram {board, rows, cols} = Nonogram {board = solution, rows, cols}
  where
    constraints =
      sortBy
        (flip $ comparing (sum . nums))
        ([RowConstraint {row = i, nums = xs} | (xs, i) <- rows `zip` [0 ..]] ++
         [ColConstraint {col = j, nums = xs} | (xs, j) <- cols `zip` [0 ..]])
    m = length rows
    n = length cols
    intervals ks n = foldM g [] ks
      where
        f k n = [i ... i + k - 1 | i <- [0 .. n - k]]
        g acc k =
          [ x : acc
          | x <- f k n
          , all
              (\y ->
                 Interval.inf x - Interval.sup y > 1 ||
                 Interval.inf y - Interval.sup x > 1)
              acc
          ]
    points = foldl' f Set.empty
      where
        f z x = Set.union z (Set.fromList [Interval.inf x .. Interval.sup x])
    solution =
      fst . head $ foldM f (Set.empty, (Set.empty, Set.empty)) constraints
      where
        f (z, (r, c)) RowConstraint {row, nums} =
          [(Set.union z x, (Set.insert row r, c)) | x <- xs]
          where
            y =
              Set.fromList [col | col <- [0 .. n - 1], Set.member (row, col) z]
            xs =
              map
                (Set.map (\col -> (row, col)))
                [ x'
                | x <- intervals nums n
                , let x' = points x
                , Set.null (Set.difference y x')
                , Set.null (Set.intersection c (Set.difference x' y))
                ]
        f (z, (r, c)) ColConstraint {col, nums} =
          [(Set.union z x, (r, Set.insert col c)) | x <- xs]
          where
            y =
              Set.fromList [row | row <- [0 .. m - 1], Set.member (row, col) z]
            xs =
              map
                (Set.map (\row -> (row, col)))
                [ x'
                | x <- intervals nums m
                , let x' = points x
                , Set.null (Set.difference y x')
                , Set.null (Set.intersection r (Set.difference x' y))
                ]

main :: IO ()
main = do
  let nonogram =
        fromRowsCols
          [[3], [2, 1], [3, 2], [2, 2], [6], [1, 5], [6], [1], [2]]
          [[1, 2], [3, 1], [1, 5], [7, 1], [5], [3], [4], [3]]
  print $ solve nonogram
