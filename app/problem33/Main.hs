coprime :: Int -> Int -> Bool
coprime x y = gcd x y == 1

main :: IO ()
main = print $ coprime 35 64
