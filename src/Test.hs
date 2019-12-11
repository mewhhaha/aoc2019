module Test (run) where

import Data.List

pretty :: (Show a, Show b, Eq b) => (a, b) -> b -> IO ()
pretty (q, a) r = putStrLn $ show q ++ " => " ++ show a ++ ": " ++ if r == a then "Pass" else "Failed with " ++ show r

run :: (Show a, Show b, Eq b) => (a -> b) -> [(a, b)] -> IO ()
run f = sequence_ . intercalate [putStrLn ""] . fmap (\t@(q, _) -> [pretty t (f q)])
