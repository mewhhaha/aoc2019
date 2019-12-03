{-# LANGUAGE LambdaCase #-}
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
    go m0 mem =
      let [m1, m2, m3, x, y] = (mem Map.!) <$> [m0 + 1, m0 + 2, m0 + 3, m1, m2]
          apply op = go (m0 + 4) $ Map.insert m3 (x `op` y) mem
       in case mem Map.! m0 of
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

-- Day 3

data Wire = R Int | L Int | U Int | D Int

day3examples =
  [ ( [ [R 8, U 5, L 5, D 3],
        [U 7, R 6, D 4, L 4]
      ],
      6
    ),
    ( [ [R 75, D 30, R 83, U 83, L 12, D 49, R 71, U 7, L 72],
        [U 62, R 66, U 55, R 34, D 71, R 55, D 58, R 83]
      ],
      159
    ),
    ( [ [R 98, U 47, R 26, D 63, R 33, U 87, L 62, D 20, R 33, U 53, R 51],
        [U 98, R 91, D 20, R 16, D 67, R 40, U 7, R 15, U 6, R 7]
      ],
      135
    )
  ]

follow = go Map.empty
  where
    go board xs = undefined
    draw (x, y) =
      let draw xs ys = Map.fromList (zip (zip xs ys) (repeat [1]))
          drawX to = draw [x .. to] (repeat y)
          drawY to = draw (repeat x) [y .. to]
       in \case
            R d -> drawX (x + d)
            L d -> drawX (x - d)
            U d -> drawY (y + d)
            D d -> drawY (y - d)

day3 = do
  test follow day3examples
