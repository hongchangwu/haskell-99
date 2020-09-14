import Control.Arrow (first)
import Control.Monad (guard)
import Data.Ratio

data Expr
  = Val Int
  | Add Expr Expr
  | Sub Expr Expr
  | Mul Expr Expr
  | Div Expr Expr

class Precedence a where
  precedence :: a -> Int

instance Precedence Expr where
  precedence (Val _) = 0
  precedence (Add _ _) = 6
  precedence (Sub _ _) = 6
  precedence (Mul _ _) = 7
  precedence (Div _ _) = 7

showsOp :: Int -> Int -> String -> Expr -> Expr -> ShowS
showsOp d p op x y =
  showParen (d > p) $ showsPrec p x . showString op . showsPrec (succ p) y

instance Show Expr where
  showsPrec _ (Val x) = shows x
  showsPrec d e@(Add x y) = showsOp d (precedence e) "+" x y
  showsPrec d e@(Sub x y) = showsOp d (precedence e) "-" x y
  showsPrec d e@(Mul x y) = showsOp d (precedence e) "*" x y
  showsPrec d e@(Div x y) = showsOp d (precedence e) "/" x y

newtype Equation = Equation (Expr, Expr)

instance Show Equation where
  show (Equation (l, r)) = show l ++ " = " ++ show r

eval :: Expr -> Ratio Int
eval (Val x) = x % 1
eval (Add x y) = eval x + eval y
eval (Sub x y) = eval x - eval y
eval (Mul x y) = eval x * eval y
eval (Div x y) = eval x / eval y

partitions :: [a] -> [([a], [a])]
partitions [] = []
partitions [_] = []
partitions (x : xs) =
  ([x], xs) : map (first (x :)) (partitions xs)

rightAssociative :: Expr -> Bool
rightAssociative (Add _ (Add _ _)) = True
rightAssociative (Add _ (Sub _ _)) = True
rightAssociative (Mul _ (Mul _ _)) = True
rightAssociative (Mul _ (Div _ _)) = True
rightAssociative _ = False

divByZero :: Expr -> Bool
divByZero (Div _ (Val 0)) = True
divByZero _ = False

expressions :: [Int] -> [Expr]
expressions [] = []
expressions [x] = [Val x]
expressions xs = do
  (us, vs) <- partitions xs
  x <- expressions us
  y <- expressions vs
  op <- [Add, Sub, Mul, Div]
  let expr = op x y
  guard $ not (rightAssociative expr || divByZero expr)
  return expr

puzzle :: [Int] -> [String]
puzzle xs = map show solutions
  where
    solutions = do
      (us, vs) <- partitions xs
      x <- expressions us
      y <- expressions vs
      let equation = Equation (x, y)
      guard $ eval x == eval y
      return equation

main :: IO ()
main = mapM_ putStrLn $ puzzle [2, 3, 5, 7, 11]
