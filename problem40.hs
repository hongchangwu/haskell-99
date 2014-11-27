import Number (primesTME, isPrime)

goldbach :: Integral a => a -> (a, a)
goldbach n
  | n <= 2 || odd n = error "Please give an even number greater than 2"
  | otherwise = search n (tail primesTME) 
  where search n [] = error "Congratulations, you just disapprove the Goldbach's conjecture"
        search n (x:xs) = case isPrime (n - x) of
          True  -> (x, n - x)
          False -> search n xs
          
