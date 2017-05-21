import           Data.Function ((&))
import           Data.List     (partition)
import           Prelude       hiding (cycle)

cycle :: Eq a => a ->  [(a, a)] -> [[a]]
cycle x = recur x
  where
    recur u [] = []
    recur u es = xs ++ (map snd us >>= flip recur vs & map (u :))
      where
        (us, vs) = partition ((==) u . fst) es
        xs = us & filter ((== x) . snd) & map (\(a, b) -> [a, b])

main :: IO ()
main = do
  print $ cycle 2 [(1,2),(2,3),(1,3),(3,4),(4,2),(5,6)]
  print $ cycle 1 [(1,2),(2,3),(1,3),(3,4),(4,2),(5,6)]
