import Control.Applicative (liftA2)
import Data.List (partition)

paths :: Eq a => a -> a -> [(a, a)] -> [[a]]
paths _ _ [] = []
paths x y es = xs ++ (map snd us >>= \w -> [x : ws | ws <- paths w y vs])
  where
    (us, vs) = partition ((==) x . fst) es
    xs = map (\(a, b) -> [a, b]) . filter ((== y) . snd) $ us
