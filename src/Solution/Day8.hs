module Solution.Day8 where

import Data.List
import Data.List.Split

input :: IO [String]
input = chunksOf (25 * 6) <$> readFile "input/day8.txt"

-- Question 1

leastZeros :: [String] -> (Int, String)
leastZeros = minimum . (fmap (length . filter (== '0')) >>= zip)

solve1 :: IO ()
solve1 =
  do
    layers <- input
    let result =
          (\s -> product $ length . (`filter` s) . (==) <$> ['1', '2'])
            . snd
            . leastZeros
            $ layers
    print result

-- Question 2

apply :: [String] -> String
apply = fmap (head . dropWhile (== '2')) . transpose

format :: String -> String
format = unlines . chunksOf 25 . fmap (\x -> if x == '0' then ' ' else '#')

solve2 :: IO ()
solve2 = do
  layers <- input
  let result = format $ apply layers
  putStr result
