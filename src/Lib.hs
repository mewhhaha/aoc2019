{-# LANGUAGE ViewPatterns #-}

module Lib where

import Data.List
import Data.List.Split
import qualified Data.Map as Map
import Data.Maybe
import Debug.Trace

pretty (a, b) q p = putStrLn $ show a ++ " => " ++ show b ++ ": " ++ if q == p then show True else show q

test :: (Show a, Show b, Eq b) => (a -> b) -> [(a, b)] -> IO ()
test f = mapM_ (\t@(q, a) -> pretty t (f q) a)

-- Day 1
day1examples =
  [ (3, 0),
    (12, 2),
    (14, 2),
    (1969, 654),
    (100756, 33583)
  ]

fuelreq :: Integer -> Integer
fuelreq = max 0 . (subtract 2) . (`div` 3)

fuelreqrec = sum . takeWhile (> 0) . iterate fuelreq . fuelreq

day1 :: IO ()
day1 = do
  -- test fuelreq day1examples
  numbers <- lines <$> readFile "input/day1.txt"
  let result = sum . fmap (fuelreqrec . read) $ numbers
  putStrLn . show $ result

-- Day 2
day2examples = [([1, 9, 10, 3, 2, 3, 11, 0, 99, 30, 40, 50], 3500)]

interpret :: Int -> Int -> Map.Map Int Int -> Int
interpret noun verb = go 0 . Map.insert 1 noun . Map.insert 2 verb
  where
    go i mem =
      let [m1, m2, m3, x, y] = (mem Map.!) <$> [i + 1, i + 2, i + 3, m1, m2]
          apply op = go (i + 4) $ Map.insert m3 (x `op` y) mem
       in case mem Map.! i of
            1 -> apply (+)
            2 -> apply (*)
            99 -> finish
              where
                finish = mem Map.! 0

memory = Map.fromList . zip [0 ..]

day2 :: IO ()
day2 = do
  -- test interpret day2examples
  mem <- memory . fmap read . splitOn "," <$> readFile "input/day2.txt"
  -- let result = interpret 12 2 mem
  let result =
        head
          [ 100 * n + v
            | n <- [0 .. 99],
              v <- [0 .. 99],
              interpret n v mem == 19690720
          ]
  putStrLn . show $ result
