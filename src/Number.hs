module Number where

import Data.List (union)

{-# OPTIONS_GHC -O2 -fno-cse #-}
-- tree-merging Eratosthenes sieve
-- producing infinite list of all prime numbers
-- https://wiki.haskell.org/Prime_numbers
primesTME :: Integral a => [a]
primesTME = 2 : gaps 3 (join [[p * p,p * p + 2 * p ..] | p <- primes'])
  where
    primes' = 3 : gaps 5 (join [[p * p,p * p + 2 * p ..] | p <- primes'])
    join ((x:xs):t) = x : union xs (join (pairs t))
    pairs ((x:xs):ys:t) = (x : union xs ys) : pairs t
    gaps k xs@(x:t)
      | k == x = gaps (k + 2) t
      | True = k : gaps (k + 2) xs

isPrime :: Integral a => a -> Bool
isPrime k =
  k > 1 && foldr (\p r -> p * p > k || k `rem` p /= 0 && r) True primesTME

primesR :: Integral a => a -> a -> [a]
primesR l r
  | r < 100 || l * l < r =
    case odd l of
      True -> [x | x <- [l,l + 2 .. r], isPrime x]
      _ -> [x | x <- [l + 1,l + 3 .. r], isPrime x]
  | otherwise = takeWhile (<= r) . dropWhile (< l) $ primesTME
