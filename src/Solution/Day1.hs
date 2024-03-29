module Solution.Day1 where

import qualified Test

input :: IO [Integer]
input = fmap read . lines <$> readFile "input/day1.txt"

-- Question 1

day1examples =
  [ (3, 0),
    (12, 2),
    (14, 2),
    (1969, 654),
    (100756, 33583)
  ]

fuelreq :: Integer -> Integer
fuelreq = max 0 . (\x -> x `div` 3 - 2)

test :: IO ()
test = Test.run fuelreq day1examples

solve1 :: IO ()
solve1 = do
  result <- sum . fmap fuelreq <$> input
  print result

-- Question 2

fuelreqrec :: Integer -> Integer
fuelreqrec = sum . takeWhile (> 0) . tail . iterate fuelreq

solve2 :: IO ()
solve2 = do
  result <- sum . fmap fuelreqrec <$> input
  print result
