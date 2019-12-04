module Solution.Day4 where

import Data.List
import Test

(from, to) = (125730, 579381)

-- Question 1

day4examples1 =
  [ (111111, True),
    (223450, False),
    (123789, False)
  ]

adjacent :: Int -> Bool
adjacent = any ((> 1) . length) . group . show

increase :: Int -> Bool
increase = (==) <$> show <*> sort . show

valid1 :: Int -> Bool
valid1 n = all ($ n) [adjacent, increase]

test1 :: IO ()
test1 = Test.run valid1 day4examples1

solve1 :: IO ()
solve1 =
  let result = sum [1 | n <- [from .. to], valid1 n]
   in putStrLn $ show result

-- Question 2

day4examples2 =
  [ (112233, True),
    (123444, False),
    (111122, True)
  ]

doubles :: Int -> Bool
doubles = any ((== 2) . length) . group . show

valid2 :: Int -> Bool
valid2 n = all ($ n) [doubles, increase]

test2 :: IO ()
test2 = Test.run valid2 day4examples2

solve2 :: IO ()
solve2 =
  let result = sum [1 | n <- [from .. to], valid2 n]
   in putStrLn $ show result
