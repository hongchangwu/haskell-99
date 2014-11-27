import Logic

table :: (Bool -> Bool -> Bool) -> IO ()
table f = mapM_ printTable [(a, b) | a <- [True, False], b <- [True, False]]
  where printTable (a, b) = putStrLn $ show a ++ " " ++ show b ++ " " ++ show (f a b)
