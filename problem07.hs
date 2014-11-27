data NestedList a = Elem a | List [NestedList a]
flatten :: NestedList x -> [x]
flatten (Elem x) = [x]
flatten (List x) = foldl (++) [] $ map flatten x

