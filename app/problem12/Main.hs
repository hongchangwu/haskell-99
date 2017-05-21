data SingleOrMultiple a = Multiple Int a | Single a deriving (Show)

decodeModified :: Eq x => [SingleOrMultiple x] -> [x]
decodeModified = concatMap decodeHelper
  where
    decodeHelper (Single x)     = [x]
    decodeHelper (Multiple n x) = replicate n x

main :: IO ()
main =
  print $
  decodeModified
    [ Multiple 4 'a'
    , Single 'b'
    , Multiple 2 'c'
    , Multiple 2 'a'
    , Single 'd'
    , Multiple 4 'e'
    ]
