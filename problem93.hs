import Data.Ratio

data Expr
  = Val Int
  | Add Expr
        Expr
  | Sub Expr
        Expr
  | Mul Expr
        Expr
  | Div Expr
        Expr

instance Show Expr where
  show (Val x) = show x
  show (Add x y) = "(+ " ++ show x ++ " " ++ show y ++ ")"
  show (Sub x y) = "(- " ++ show x ++ " " ++ show y ++ ")"
  show (Mul x y) = "(* " ++ show x ++ " " ++ show y ++ ")"
  show (Div x y) = "(/ " ++ show x ++ " " ++ show y ++ ")"

data Equation = Equation Expr Expr

instance Show Equation where
  show (Equation l r) = show l ++ " = " ++ show r

eval :: Expr -> Ratio Int
eval (Val x) = x % 1
eval (Add x y) = eval x + eval y
eval (Sub x y) = eval x - eval y
eval (Mul x y) = eval x * eval y
eval (Div x y) = eval x / eval y

applyFst :: (a -> b) -> (a, c) -> (b, c)
applyFst f (x, y) = (f x, y)

partitions :: [a] -> [([a], [a])]
partitions [] = []
partitions [_] = []
partitions (x:xs) =
  ([x], xs) : map (applyFst (x :)) (partitions xs)

expressions :: [Int] -> [Expr]
expressions [] = []
expressions [x] = [Val x]
expressions xs = do
  (us, vs) <- partitions xs
  x <- expressions us
  y <- expressions vs
  op <- [Add, Sub, Mul, Div]
  return $ op x y

puzzle :: [Int] -> [String]
puzzle xs =  map show .  filter valid $ candidates
  where
    valid (Equation x y) = eval x == eval y
    candidates = do
      (us, vs) <- partitions xs
      x <- expressions us
      y <- expressions vs
      return $ Equation x y

main :: IO ()
main = mapM_ putStrLn $ puzzle [2, 3, 5, 7, 11]