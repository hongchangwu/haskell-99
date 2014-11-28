module Tree where

import Data.List (nub)

data Tree a = Empty | Branch a (Tree a) (Tree a)
            deriving (Show, Eq, Read)

leaf :: a -> Tree a
leaf x = Branch x Empty Empty                     

trees :: Eq a => a -> Int -> [Tree a]
trees _ 0 = []
trees x 1 = [leaf x]
trees x n = nub $ concat [insert x t | t <- trees x (n - 1)]

insert :: a -> Tree a -> [Tree a]
insert x Empty = [leaf x]
insert x (Branch y u v) = [Branch y u' v | u' <- insert x u] ++ [Branch y u v' | v' <- insert x v]

insert' :: Ord a => a -> Tree a -> Tree a
insert' x Empty = leaf x
insert' x t@(Branch y u v) 
  | x < y = (Branch y (insert' x u) v)
  | x > y = (Branch y u (insert' x v))
  | otherwise = t
                
symmetric :: Tree a -> Bool
symmetric Empty = True
symmetric (Branch _ u v) = mirror u v
  where mirror Empty Empty = True
        mirror (Branch _ uu uv) (Branch _ vu vv) = mirror uu vv && mirror uv vu
        mirror _ _ = False
                
isBalanced :: Tree a -> Bool
isBalanced Empty = True
isBalanced (Branch a u v) = abs(count u - count v) <= 1

count :: Tree a -> Int
count Empty = 0
count (Branch a u v) = 1 + (count u + count v)

tree4 = Branch 1 (Branch 2 Empty (Branch 4 Empty Empty))
                 (Branch 2 Empty Empty)
        
mapTree :: (a -> a) -> Tree a -> Tree a
mapTree _ Empty = Empty
mapTree f (Branch x l r) = Branch (f x) (mapTree f l) (mapTree f r)

height :: Tree a -> Int
height Empty = 0
height (Branch a l r) = 1 + max (height l) (height r)
