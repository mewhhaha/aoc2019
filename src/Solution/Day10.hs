{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}

module Solution.Day10 where

import Control.Arrow
import Data.Bool
import Data.List
import Data.List.Split
import qualified Data.Map as Map
import Data.Maybe
import Data.Monoid
import Data.Ratio
import Debug.Trace
import Test

data Space = Empty | Asteroid
  deriving (Show, Ord, Eq)

both f = first f >>> second f

coordinates :: [String] -> Map.Map (Int, Int) Space
coordinates = Map.fromList . concat . zipWith points [0 ..]

points :: Int -> String -> [((Int, Int), Space)]
points y = zipWith (curry $ (,y) *** space) [0 ..]

space :: Char -> Space
space c = if c == '.' then Empty else Asteroid

input :: IO (Map.Map (Int, Int) Space)
input = coordinates . lines <$> readFile "input/day10.txt"

isEmpty :: Space -> Bool
isEmpty Empty = True
isEmpty _ = False

minMax :: (Int, Int) -> (Int, Int)
minMax (x, y) = (min x y, max x y)

range :: Int -> Int -> (Int -> Maybe (Int, Int)) -> [(Int, Int)]
range a b f = catMaybes $ f <$> [(min a b + 1) .. (max a b - 1)]

sign x = if x > 0 then 1 else -1

intersections :: (Int, Int) -> (Int, Int) -> [(Int, Int)]
intersections (x, y) (x', y')
  | y == y' = range x x' (pure . (,y))
  | x == x' = range y y' (pure . (x,))
  | otherwise = range y y' calcX
  where
    calcX y'' =
      let dir = if sign (x - x') /= sign (y - y') then max y y' - y'' else y'' - min y y'
          num = abs (x - x') * dir
          den = abs (y - y')
       in if num `mod` den == 0 then Just (num `div` den + min x x', y'') else Nothing

sightOf :: Map.Map (Int, Int) Space -> (Int, Int) -> (Int, Int) -> Bool
sightOf grid from to = all (isEmpty . (grid Map.!)) $ intersections from to

mostSightOf :: Map.Map (Int, Int) Space -> (Int, (Int, Int))
mostSightOf grid = maximum $ bySights <$> asteroids
  where
    asteroids = [coord | (coord, s) <- Map.toList grid, not . isEmpty $ s]
    bySights coord = (,coord) . length . filter (\a -> a /= coord && sightOf grid coord a) $ asteroids

day10examples =
  [ ( coordinates
        [ ".#..#",
          ".....",
          "#####",
          "....#",
          "...##"
        ],
      (8, (3, 4))
    ),
    ( coordinates
        [ "#.........",
          "...A......",
          "...B..a...",
          ".EDCG....a",
          "..F.c.b...",
          ".....c....",
          "..efd.c.gb",
          ".......c..",
          "....f...c.",
          "...e..d..c"
        ],
      (6, (0, 0))
    ),
    ( coordinates
        [ "......#.#.",
          "#..#.#....",
          "..#######.",
          ".#.#.###..",
          ".#..#.....",
          "..#....#.#",
          "#..#....#.",
          ".##.#..###",
          "##...#..#.",
          ".#....####"
        ],
      (33, (5, 8))
    ),
    ( coordinates
        [ "#.#...#.#.",
          ".###....#.",
          ".#....#...",
          "##.#.#.#.#",
          "....#.#.#.",
          ".##..###.#",
          "..#...##..",
          "..##....##",
          "......#...",
          ".####.###."
        ],
      (35, (1, 2))
    ),
    ( coordinates
        [ ".#..#..###",
          "####.###.#",
          "....###.#.",
          "..###.##.#",
          "##.##.#.#.",
          "....###..#",
          "..#.#..#.#",
          "#..#.#.###",
          ".##...##.#",
          ".....#.#.."
        ],
      (41, (6, 3))
    ),
    ( coordinates
        [ ".#..##.###...#######",
          "##.############..##.",
          ".#.######.########.#",
          ".###.#######.####.#.",
          "#####.##.#.##.###.##",
          "..#####..#.#########",
          "####################",
          "#.####....###.#.#.##",
          "##.#################",
          "#####.##.###..####..",
          "..######..##.#######",
          "####.##.####...##..#",
          ".#####..#.######.###",
          "##...#.##########...",
          "#.##########.#######",
          ".####.#.###.###.#.##",
          "....##.##.###..#####",
          ".#.#.###########.###",
          "#.#.#.#####.####.###",
          "###.##.####.##.#..##"
        ],
      (210, (11, 13))
    )
  ]

test1 :: IO ()
test1 = Test.run mostSightOf day10examples

t =
  mapM_
    print
    $ transpose
    $ chunksOf 10
    $ fmap
      ( \case
          Empty -> '.'
          Asteroid -> '#'
          . snd
      )
    $ sort
    $ Map.toList
    $ Map.mapWithKey
      ( \c x -> case x of
          Empty -> Empty
          Asteroid -> if sightOf testgrid (6, 3) c then Empty else Asteroid
      )
      testgrid

testgrid =
  coordinates
    [ ".#..#..###",
      "####.###.#",
      "....###.#.",
      "..###.##.#",
      "##.##.#.#.",
      "....###..#",
      "..#.#..#.#",
      "#..#.#.###",
      ".##...##.#",
      ".....#.#.."
    ]

solve1 :: IO ()
solve1 = do
  spaces <- input
  print $ mostSightOf spaces
