import           Data.Char (isAlpha, isAlphaNum)

identifier :: String -> Bool
identifier [] = False
identifier (x:xs)
  | isAlpha x = check xs
  | otherwise = False
  where
    check []        = True
    check ('-':xs') = check' xs'
    check xs'       = check' xs'
    check' [] = False
    check' (x':xs')
      | isAlphaNum x' = check xs'
      | otherwise = False

main :: IO ()
main = do
  print $ identifier "this-is-a-long-identifier"
  print $ identifier "this-ends-in-"
  print $ identifier "two--hyphens"
