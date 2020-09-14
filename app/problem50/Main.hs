import Data.Heap (Heap)
import qualified Data.Heap as Heap
import Data.List
import Data.Ord (comparing)

data HTree a
  = Leaf a
  | Branch (HTree a) (HTree a)

newtype HItem w a = HItem (w, HTree a)

instance Eq w => Eq (HItem w a) where
  HItem (w1, _) == HItem (w2, _) = w1 == w2

instance Ord w => Ord (HItem w a) where
  HItem (w1, _) `compare` HItem (w2, _) = w1 `compare` w2

huffman :: (Ord a, Ord w, Num w) => [(a, w)] -> [(a, String)]
huffman xs =
  sortBy (comparing fst) . encode . htree . Heap.fromList $
    [HItem (w, Leaf x) | (x, w) <- xs]
  where
    htree ys =
      case Heap.uncons ys of
        Just (HItem (w1, t1), ys') ->
          case Heap.uncons ys' of
            Nothing -> t1
            Just (HItem (w2, t2), ys'') ->
              htree $ Heap.insert (HItem (w1 + w2, Branch t1 t2)) ys''
    encode (Branch l r) =
      [(x, '0' : code) | (x, code) <- encode l]
        ++ [(x, '1' : code) | (x, code) <- encode r]
    encode (Leaf x) = [(x, "")]

main :: IO ()
main =
  print $
    huffman [('a', 45), ('b', 13), ('c', 12), ('d', 16), ('e', 9), ('f', 5)]
