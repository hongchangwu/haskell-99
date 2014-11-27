import Data.List (tails)

combination :: Int -> [a] -> [([a],[a])]
combination 0 xs     = [([],xs)]
combination n []     = []
combination n (x:xs) = ts ++ ds
  where
    ts = [(x:ys,zs) | (ys,zs) <- combination (n-1) xs]
    ds = [(ys,x:zs) | (ys,zs) <- combination n     xs]

groupn :: [Int] -> [a] -> [[[a]]]
groupn [] _      = [[]]
groupn (n:ns) xs =   [ g:gs | 
                       (g,rs) <- combination n xs,
                       gs     <- groupn ns rs ]
                               
                               

