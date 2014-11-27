data SingleOrMultiple a = Multiple Int a | Single a deriving (Show)

decodeModified :: Eq x => [SingleOrMultiple x] -> [x]
decodeModified = concatMap decodeHelper
  where
    decodeHelper (Single x) = [x]
    decodeHelper (Multiple n x) = replicate n x

