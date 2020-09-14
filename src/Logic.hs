module Logic where

and' :: Bool -> Bool -> Bool
and' = (&&)

or' :: Bool -> Bool -> Bool
or' = (||)

nand' :: Bool -> Bool -> Bool
nand' a b = not $ and' a b

nor' :: Bool -> Bool -> Bool
nor' a b = not $ or' a b

xor' :: Bool -> Bool -> Bool
xor' = (/=)

impl' :: Bool -> Bool -> Bool
impl' a b = not $ a && not b

equ' :: Bool -> Bool -> Bool
equ' = (==)

infixl 4 `or'`

infixl 6 `and'`

infixl 3 `equ'`
-- infixl 7 `equ'`
