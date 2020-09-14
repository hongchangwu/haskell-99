import MultiTree

ipl :: MultiTree a -> Int
ipl (Node _ ss) = length ss + sum (map calc ss)
  where
    calc t@(Node _ ss) = ipl t + length ss

main :: IO ()
main = do
  print $ ipl tree5
  print $ ipl tree4
