module Test (run) where

pretty :: (Show a, Show b, Eq b) => (a, b) -> b -> IO ()
pretty (q, a) r = putStrLn $ show q ++ " => " ++ show a ++ ": " ++ if r == a then show True else show r

run :: (Show a, Show b, Eq b) => (a -> b) -> [(a, b)] -> IO ()
run f = mapM_ (\t@(q, _) -> pretty t (f q))
