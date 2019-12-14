module Solution.Day12 where

import Control.Arrow
import Control.Parallel.Strategies
import Data.List
import Data.List.Split
import Data.Maybe
import Data.Monoid
import Data.Ratio
import qualified Data.Set as Set
import Debug.Trace
import Test

data Planet = Planet {getName :: String, getPosition :: (Integer, Integer, Integer), getVelocity :: (Integer, Integer, Integer)}
  deriving (Eq, Show, Ord)

io :: (Integer, Integer, Integer) -> Planet
io pos = Planet "Io" pos (0, 0, 0)

europa :: (Integer, Integer, Integer) -> Planet
europa pos = Planet "Europa" pos (0, 0, 0)

ganymede :: (Integer, Integer, Integer) -> Planet
ganymede pos = Planet "Ganymede" pos (0, 0, 0)

callisto :: (Integer, Integer, Integer) -> Planet
callisto pos = Planet "Callisto" pos (0, 0, 0)

planets :: [Planet]
planets = [io (-14, -4, -11), europa (-9, 6, -7), ganymede (4, 1, 4), callisto (2, -14, -9)]

sign :: Integer -> Integer
sign 0 = 0
sign x = if x < 0 then -1 else 1

instance (Num a, Num b, Num c) => Num (a, b, c) where

  fromInteger a = (fromInteger a, fromInteger a, fromInteger a)

  (a, b, c) + (a', b', c') = (a + a', b + b', c + c')

update :: Planet -> [Planet] -> Planet
update p = velocity p . sum . fmap (gravity p)
  where
    gravity (Planet _ (x, y, z) _) (Planet _ (x', y', z') _) = (sign $ x' - x, sign $ y' - y, sign $ z' - z)
    velocity (Planet n p v) w = let v' = v + w in Planet n (p + v') v'

step :: [Planet] -> [Planet]
step ps = [update p (filter (/= p) ps) | p <- ps]

steps :: Integer -> [Planet] -> [Planet]
steps n = (!! fromInteger n) . iterate step

energy :: [Planet] -> Integer
energy = sum . fmap ((*) <$> pot <*> kin)
  where
    calc (a, b, c) = abs a + abs b + abs c
    pot (Planet _ pos _) = calc pos
    kin (Planet _ _ vel) = calc vel

day12examples1 =
  [ ( ( 10,
        [ io (-1, 0, 2),
          europa (2, -10, -7),
          ganymede (4, -8, 8),
          callisto (3, 5, -1)
        ]
      ),
      179
    ),
    ( ( 100,
        [ io (-8, -10, 0),
          europa (5, 5, 10),
          ganymede (2, -7, 3),
          callisto (9, -8, -3)
        ]
      ),
      1940
    )
  ]

test1 :: IO ()
test1 = Test.run (energy . uncurry steps) day12examples1

solve1 :: IO ()
solve1 = do
  let result = energy $ steps 1000 planets
  print result

-- Question 2

untilRepeat :: [Planet] -> Integer
untilRepeat start =
  getN
    . dropWhile ((/= start) . snd)
    . tail
    $ zip [1 ..] (iterate step (step start))
  where
    getN = fst . head

onlyX (Planet n (x, _, _) v) = Planet n (x, 0, 0) v

onlyY (Planet n (_, y, _) v) = Planet n (0, y, 0) v

onlyZ (Planet n (_, _, z) v) = Planet n (0, 0, z) v

untilRepeatAxis :: [Planet] -> [Integer]
untilRepeatAxis =
  (untilRepeat <$>)
    . zipWith fmap [onlyX, onlyY, onlyZ]
    . replicate 3

converge :: [Integer] -> Integer
converge = foldl lcm 1

day12examples2 =
  [ ( [ io (-1, 0, 2),
        europa (2, -10, -7),
        ganymede (4, -8, 8),
        callisto (3, 5, -1)
      ],
      2772
    ),
    ( [ io (-8, -10, 0),
        europa (5, 5, 10),
        ganymede (2, -7, 3),
        callisto (9, -8, -3)
      ],
      4686774924
    )
  ]

repeatsAt :: [Planet] -> Integer
repeatsAt = converge . (`using` parList rdeepseq) . untilRepeatAxis

test2 :: IO ()
test2 = Test.run repeatsAt day12examples2

solve2 :: IO ()
solve2 = do
  let n = repeatsAt planets
  print n
