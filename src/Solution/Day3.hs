module Solution.Day3 where

import Data.List
import Data.List.Split
import qualified Data.Map as Map
import qualified Test

data Wire = R Int | L Int | U Int | D Int
  deriving (Show, Read)

parse :: String -> Wire
parse (d : n) = read ([d] ++ " " ++ n)

input :: IO [[Wire]]
input = fmap (fmap parse . splitOn ",") . lines <$> readFile "input/day3.txt"

-- Question 1

day3examples1 =
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

manhattan :: (Int, Int) -> Int
manhattan (x, y) = abs x + abs y

sign :: Int -> Int
sign x
  | x < 0 = -1
  | x > 0 = 1
  | otherwise = 0

points :: (Int, Int) -> (Int, Int) -> [(Int, Int)]
points (x, y) to@(x', y')
  | x /= x' = next (x + sign (x' - x), y)
  | y /= y' = next (x, y + sign (y' - y))
  | otherwise = []
  where
    next coords = coords : points coords to

follow :: [Wire] -> Map.Map (Int, Int) [Int]
follow = go 1 (0, 0)
  where
    go i _ [] = mempty
    go i pos@(x, y) (w : ws) =
      let to = case w of
            R d -> (x + d, y)
            L d -> (x - d, y)
            U d -> (x, y + d)
            D d -> (x, y - d)
          line = points pos to
          wire = Map.fromList $ zip line ((: []) <$> [i, i + 1 ..])
       in Map.union wire $ go (i + length line) to ws

crossings :: [Map.Map (Int, Int) [Int]] -> [((Int, Int), [Int])]
crossings = filter ((> 1) . length . snd) . Map.toList . foldl1 (Map.unionWith (++))

crossingBy :: Ord a => (((Int, Int), [Int]) -> a) -> [[Wire]] -> a
crossingBy f = minimum . fmap f . crossings . fmap follow

closest :: [[Wire]] -> Int
closest = crossingBy (manhattan . fst)

test1 :: IO ()
test1 = Test.run closest day3examples1

solve1 :: IO ()
solve1 = do
  result <- closest <$> input
  print result

-- Question 2

day3examples2 =
  [ ( [ [R 8, U 5, L 5, D 3],
        [U 7, R 6, D 4, L 4]
      ],
      30
    ),
    ( [ [R 75, D 30, R 83, U 83, L 12, D 49, R 71, U 7, L 72],
        [U 62, R 66, U 55, R 34, D 71, R 55, D 58, R 83]
      ],
      610
    ),
    ( [ [R 98, U 47, R 26, D 63, R 33, U 87, L 62, D 20, R 33, U 53, R 51],
        [U 98, R 91, D 20, R 16, D 67, R 40, U 7, R 15, U 6, R 7]
      ],
      410
    )
  ]

test2 :: IO ()
test2 = Test.run fastest day3examples2

fastest :: [[Wire]] -> Int
fastest = crossingBy (sum . snd)

solve2 :: IO ()
solve2 = do
  result <- fastest <$> input
  print result
