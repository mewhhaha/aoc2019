module Solution.Day2 where

import Data.List.Split
import qualified Data.Map as Map
import qualified Test

memory :: [Int] -> Map.Map Int Int
memory = Map.fromList . zip [0 ..]

input :: IO (Map.Map Int Int)
input = memory . fmap read . splitOn "," <$> readFile "input/day2.txt"

-- Question 1

day2examples =
  [ (memory [1, 9, 10, 3, 2, 3, 11, 0, 99, 30, 40, 50], 3500)
  ]

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

test :: IO ()
test = Test.run (interpret 9 10) day2examples

solve1 :: IO ()
solve1 = do
  result <- interpret 12 2 <$> input
  putStrLn . show $ result

-- Question 2

solve2 :: IO ()
solve2 = do
  mem <- input
  let result =
        head
          [ 100 * n + v
            | n <- [0 .. 99],
              v <- [0 .. 99],
              interpret n v mem == 19690720
          ]
  putStrLn . show $ result
