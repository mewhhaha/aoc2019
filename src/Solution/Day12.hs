{-# LANGUAGE TypeApplications #-}

module Solution.Day12 where

import Data.Monoid

data Planet = Planet (Int, Int, Int) (Int, Int, Int)

io :: Planet
io = Planet (-14, 4, -11) (0, 0, 0)

europa :: Planet
europa = Planet (-9, 6, -7) (0, 0, 0)

ganymede :: Planet
ganymede = Planet (4, 1, 4) (0, 0, 0)

callisto :: Planet
callisto = Planet (2, -14, -9) (0, 0, 0)

planets :: [Planet]
planets = [io, europa, ganymede, callisto]

sign :: Int -> Int
sign 0 = 0
sign x = if x < 0 then -1 else 1

instance (Num a, Num b, Num c) => Num (a, b, c) where
  (a, b, c) + (a', b', c') = (a + a', b + b', c + c')

step :: Planet -> [Planet] -> Planet
step p = velocity p . sum . fmap (gravity p)
  where
    gravity (Planet (x, y, z) _) (Planet (x', y', z') _) = (sign $ x - x', sign $ y - y', sign $ z - z')
    velocity (Planet p v) w = let v' = v + w in Planet (p + v') v'
