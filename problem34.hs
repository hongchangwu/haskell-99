totient :: Int -> Int
totient n 
  | n <= 0    = error "Non-positive integer"
  | n == 1    = 1
  | otherwise = length [x | x <- [1..n-1], coprime x n]
  where coprime a b = gcd a b == 1