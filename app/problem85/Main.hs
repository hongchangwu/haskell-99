import Graph

main :: IO ()
main = do
  let graphG1 =
        Graph
          [1, 2, 3, 4, 5, 6, 7, 8]
          [ (1, 5),
            (1, 6),
            (1, 7),
            (2, 5),
            (2, 6),
            (2, 8),
            (3, 5),
            (3, 7),
            (3, 8),
            (4, 6),
            (4, 7),
            (4, 8)
          ]
      graphH1 =
        Graph
          [1, 2, 3, 4, 5, 6, 7, 8]
          [ (1, 2),
            (1, 4),
            (1, 5),
            (6, 2),
            (6, 5),
            (6, 7),
            (8, 4),
            (8, 5),
            (8, 7),
            (3, 2),
            (3, 4),
            (3, 7)
          ]
  print $ iso graphG1 graphH1
