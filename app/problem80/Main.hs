import Graph

main :: IO ()
main =
  print $
    graphToAdj
      ( Graph
          ['b', 'c', 'd', 'f', 'g', 'h', 'k']
          [('b', 'c'), ('b', 'f'), ('c', 'f'), ('f', 'k'), ('g', 'h')]
      )
