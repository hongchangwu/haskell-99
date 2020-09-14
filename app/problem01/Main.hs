myLast :: [a] -> a
myLast [] = error "empty list"
myLast [x] = x
myLast (_ : xs) = myLast xs

main :: IO ()
main = do
  print $ myLast [1, 2, 3, 4]
  print $ myLast ['x', 'y', 'z']
