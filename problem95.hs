import           Data.List (intersperse, (!!))

digits :: Int -> [Int]
digits 0 = [0]
digits x = digits' [] x
  where
    digits' acc 0 = acc
    digits' acc x =
      let (d, m) = x `divMod` 10
          acc' = m : acc
      in digits' acc' d


fullWords :: Int -> String
fullWords = concat . intersperse "-" . map (dict !!) . digits
  where
    dict =
      [ "zero"
      , "one"
      , "two"
      , "three"
      , "four"
      , "five"
      , "six"
      , "seven"
      , "eight"
      , "nine"
      ]

main :: IO ()
main = putStrLn $ fullWords 175
