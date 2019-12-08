module Solution.Day6 where

import Control.Arrow
import Data.List
import Data.List.Split
import qualified Data.Map as Map
import Data.Maybe
import Test

parseOrbit :: String -> Map.Map String [String]
parseOrbit str =
  case splitOn ")" str of
    [x, o] -> Map.singleton x [o]

input :: IO (Map.Map String [String])
input = foldl1 (Map.unionWith (++)) . fmap parseOrbit . lines <$> readFile "input/day6.txt"

-- Question 1

day6examples1 =
  [ ( Map.fromList
        [ ("COM", ["B"]),
          ("B", ["G", "C"]),
          ("G", ["H"]),
          ("C", ["D"]),
          ("D", ["E", "I"]),
          ("E", ["J", "F"]),
          ("J", ["K"]),
          ("K", ["L"])
        ],
      42
    )
  ]

count :: Map.Map String [String] -> Int
count graph = go 0 (Map.lookup "COM" graph)
  where
    go depth Nothing = depth
    go depth (Just xs) = depth + (sum . fmap (go (depth + 1) . (`Map.lookup` graph)) $ xs)

test1 :: IO ()
test1 = Test.run count day6examples1

solve1 :: IO ()
solve1 = do
  result <- count <$> input
  print result

-- Question 20

day6examples2 =
  [ ( Map.fromList
        [ ("COM", ["B"]),
          ("B", ["G", "C"]),
          ("G", ["H"]),
          ("C", ["D"]),
          ("D", ["E", "I"]),
          ("E", ["J", "F"]),
          ("J", ["K"]),
          ("K", ["L"]),
          ("K", ["YOU"]),
          ("I", ["SAN"])
        ],
      4
    )
  ]

path :: Map.Map String [String] -> Int
path graph = fromMaybe 0 $ go 0 (graph Map.! "COM")
  where
    go depth xs = case listToMaybe . filter (`elem` ["YOU", "SAN"]) $ xs of
      Nothing ->
        let deeper = mapMaybe (\x -> Map.lookup x graph >>= go (depth + 1)) xs
         in case deeper of
              [y, s] -> Just $ y + s - depth * 2
              ys -> listToMaybe ys
      _ -> Just depth

test2 :: IO ()
test2 = Test.run path day6examples2

solve2 :: IO ()
solve2 = do
  result <- path <$> input
  print result
