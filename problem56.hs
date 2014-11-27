import Tree

symmetric :: Tree a -> Bool
symmetric Empty = True
symmetric (Branch _ u v) = mirror u v
  where mirror Empty Empty = True
        mirror (Branch _ uu uv) (Branch _ vu vv) = mirror uu vv && mirror uv vu
        mirror _ _ = False
