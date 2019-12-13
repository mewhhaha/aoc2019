module Solution.Day12 where

import Data.Monoid
import Test

data Planet = Planet String (Int, Int, Int) (Int, Int, Int)
  deriving (Eq, Show)

io :: (Int, Int, Int) -> Planet
io pos = Planet "Io" pos (0, 0, 0)

europa :: (Int, Int, Int) -> Planet
europa pos = Planet "Europa" pos (0, 0, 0)

ganymede :: (Int, Int, Int) -> Planet
ganymede pos = Planet "Ganymede" pos (0, 0, 0)

callisto :: (Int, Int, Int) -> Planet
callisto pos = Planet "Callisto" pos (0, 0, 0)

planets :: [Planet]
planets = [io (-14, -4, -11), europa (-9, 6, -7), ganymede (4, 1, 4), callisto (2, -14, -9)]

sign :: Int -> Int
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

steps :: Int -> [Planet] -> [Planet]
steps n = (!! n) . iterate step

energy :: [Planet] -> Int
energy = sum . fmap ((*) <$> pot <*> kin)
  where
    calc (a, b, c) = abs a + abs b + abs c
    pot (Planet _ pos _) = calc pos
    kin (Planet _ _ vel) = calc vel

day12examples =
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
test1 = Test.run (energy . uncurry steps) day12examples

solve1 :: IO ()
solve1 = do
  let result = energy $ steps 1000 planets
  print result
-- Question 2
