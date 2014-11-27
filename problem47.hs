import Logic

table2 :: (Bool -> Bool -> Bool) -> IO ()
table2 f = mapM_ printTable [(a, b) | a <- [True, False], b <- [True, False]]
  where printTable (a, b) = putStrLn $ show a ++ " " ++ show b ++ " " ++ show (f a b)
        


