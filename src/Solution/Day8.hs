module Solution.Day8 where

import Data.List
import Data.List.Split

width = 25

height = 6

input :: IO [String]
input = chunksOf (width * height) <$> readFile "input/day8.txt"

-- Question 1

leastZeros :: [String] -> String
leastZeros = snd . minimum . (fmap (length . filter (== '0')) >>= zip)

solve1 :: IO ()
solve1 =
  do
    layers <- input
    let result =
          (\s -> product $ length . (`filter` s) . (==) <$> ['1', '2'])
            . leastZeros
            $ layers
    print result

-- Question 2

apply :: [String] -> String
apply = fmap (head . filter (/= '2')) . transpose

format :: String -> String
format = unlines . chunksOf width . fmap (\x -> if x == '0' then ' ' else '#')

solve2 :: IO ()
solve2 = do
  layers <- input
  let result = format $ apply layers
  putStr result
