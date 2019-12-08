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

adjacent :: String -> Bool
adjacent = any ((> 1) . length) . group

increase :: String -> Bool
increase = sort >>= (==)

validate1 :: Int -> Bool
validate1 n = all ($ show n) [adjacent, increase]

test1 :: IO ()
test1 = Test.run validate1 day4examples1

solve1 :: IO ()
solve1 =
  let result = sum [1 | n <- [from .. to], validate1 n]
   in print result

-- Question 2

day4examples2 =
  [ (112233, True),
    (123444, False),
    (111122, True)
  ]

doubles :: String -> Bool
doubles = any ((== 2) . length) . group

validate2 :: Int -> Bool
validate2 n = all ($ show n) [doubles, increase]

test2 :: IO ()
test2 = Test.run validate2 day4examples2

solve2 :: IO ()
solve2 =
  let result = sum [1 | n <- [from .. to], validate2 n]
   in print result
