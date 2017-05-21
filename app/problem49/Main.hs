gray :: Int -> [String]
gray 0 = [""]
gray n = map ('0' :) g ++ map ('1' :) (reverse g)
  where
    g = gray (n - 1)

main :: IO ()
main = print $ gray 3
