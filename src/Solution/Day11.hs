module Solution.Day11 where

import Computer
import Control.Arrow
import Data.List
import qualified Data.Map as Map
import Data.Maybe
import Debug.Trace

-- Question 1

data Rotate = L | R
  deriving (Enum)

data Direction = N | E | S | W
  deriving (Enum)

data Robot = Robot (Int, Int) Direction

data Mode = Paint | Move

data Color = Black | White
  deriving (Enum, Show)

instance Mem Color where

  toMemory = fromIntegral . fromEnum

  fromMemory = toEnum . fromInteger

instance Mem Rotate where

  toMemory = fromIntegral . fromEnum

  fromMemory = ([L, R] !!) . fromInteger

move :: Rotate -> Robot -> Robot
move r (Robot (x, y) dir) = uncurry Robot $ (update >>= (,)) (rotate dir)
  where
    offset = case r of
      L -> -1
      R -> 1
    rotate = toEnum . (`mod` 4) . (+ offset) . fromEnum
    update dir = case dir of
      N -> (x, y + 1)
      E -> (x + 1, y)
      S -> (x, y -1)
      W -> (x -1, y)

position :: Robot -> (Int, Int)
position (Robot p _) = p

runRobot :: Color -> Computer -> Map.Map (Int, Int) Color
runRobot start = go (Robot (0, 0) N) (Map.singleton (0, 0) start)
  where
    go robot floor state =
      let color = fromMaybe Black (Map.lookup (position robot) floor)
       in case continues 2 [toMemory color] state of
            ([], _) -> floor
            ([memColor, memDir], state') ->
              go
                (move (fromMemory memDir) robot)
                (Map.insert (position robot) (fromMemory memColor) floor)
                state'

panels :: Map.Map (Int, Int) Color -> Int
panels = Map.size

solve1 :: IO ()
solve1 = do
  program <- input "input/day11.txt"
  print . panels $ runRobot Black ((0, 0), program)

-- Question 2

paint :: (Int, Int) -> Map.Map (Int, Int) Color -> IO ()
paint (w, h) floor = mapM_ (putStrLn . fmap panel) painting
  where
    w½ = w `div` 2
    h½ = h `div` 2
    panel pos = case fromMaybe Black $ Map.lookup pos floor of
      Black -> '.'
      White -> '#'
    painting = fmap ((<$> [- h½ .. h½]) . flip (,)) [w½, w½ -1 .. - w½]

solve2 :: IO ()
solve2 = do
  program <- input "input/day11.txt"
  paint (100, 100) $ runRobot White ((0, 0), program)
