import           Tree

main :: IO ()
main = do
  print $ stringToTree "x(y,a(,b))"
