import           Control.Monad   (foldM)
import           Data.Char       (chr, ord)
import           Data.List       (intercalate, intersperse, sortBy, (\\))
import           Data.List.Split (chunksOf, splitOn, splitWhen)
import           Data.Ord        (comparing)
import           Data.Vector     (Vector, (!), (//))
import qualified Data.Vector     as Vector

newtype Board = Board
  { board :: Vector (Vector Int)
  }

instance Show Board where
  show =
    unlines . intercalate [divider] .
    map (intersperse blankLine . map showLine) . chunksOf 3 .
    Vector.toList . board
    where
      showLine =
        intercalate " | " .
        map (intercalate "  " . map showCell) . chunksOf 3 . Vector.toList
      showCell 0 = "."
      showCell x = return $ chr (ord '0' + x)
      blankLine = intercalate "|" $ map (`replicate` ' ') [8, 9, 8]
      divider = intercalate "+" $ map (`replicate` '-') [8, 9, 8]

instance Read Board where
  readsPrec =
    const $
    return . flip (,) "" .
    Board . Vector.fromList .
    concatMap (map readLine . concat . splitWhen ((==) ' ' . head)) .
    splitWhen ((==) '-' . head) . lines
    where
      readLine =
        Vector.fromList .
        concatMap (map readCell . concat . splitOn " ") . splitOn "|"
      readCell '.' = 0
      readCell x   = ord x - ord '0'

type Coord = (Int, Int)

vals :: [Int]
vals = [1 .. 9]

unknown :: Int -> Bool
unknown x = x == 0

unknowns :: Board -> [Coord]
unknowns (Board xss) =
  concatMap
    (\(xs, i) ->
        [ (i, j)
        | (x, j) <- Vector.toList xs `zip` [0 ..]
        , unknown x ]) $
  Vector.toList xss `zip` [0 ..]

candidates :: Board -> Coord -> [Int]
candidates b c = vals \\ knowns
  where
    knowns = filter (not . unknown) (neighbors b c)

neighbors :: Board -> Coord -> [Int]
neighbors (Board xss) (i, j) = row i ++ col j ++ box k
  where
    k = i `div` 3 * 3 + j `div` 3
    row i = Vector.toList $ xss ! i
    col j = map (\i -> xss ! i ! j) $ take (length xss) [0 ..]
    box k =
      [ xss ! i ! j
      | i <- take 3 [3 * m ..]
      , j <- take 3 [3 * n ..] ]
      where
        (m, n) = k `divMod` 3

update :: Board -> Coord -> Int -> Board
update (Board xss) (i, j) x = Board $ xss // [(i, xss ! i // [(j, x)])]

solve :: Board -> [Board]
solve b = foldM f b xs
  where
    f b' (i, j) =
      [ update b' (i, j) x
      | x <- candidates b' (i, j) ]
    xs =
      fst . unzip . sortBy (comparing (length . snd)) $
      [ ((i, j), candidates b (i, j))
      | (i, j) <- unknowns b ]

main :: IO ()
main = do
  content <- readFile "problem97.txt"
  print . head . solve $ read content
