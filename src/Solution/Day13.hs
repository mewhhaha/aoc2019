{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}

module Solution.Day13 where

import Computer
import Data.Either
import Data.List.Split
import qualified Data.Map as Map
import Data.Maybe
import System.IO (hReady, stdin)

data Tile = Empty | Wall | Block | Paddle | Ball
  deriving (Enum)

instance Show Tile where
  show = \case
    Empty -> "."
    Wall -> "H"
    Block -> "_"
    Paddle -> "~"
    Ball -> "o"

instance Mem Tile where

  toMemory = toInteger . fromEnum

  fromMemory = toEnum . fromInteger

type Window = Map.Map (Integer, Integer) Tile

-- Question 1

finish :: [Integer] -> Computer -> Window
finish input = go mempty
  where
    go window state = case continues 3 input state of
      ([x, y, memTile], state') ->
        let window' = Map.insert (x, y) (fromMemory memTile) window
         in go window' state'
      (_, state') -> window

isBlock :: Tile -> Bool
isBlock Block = True
isBlock _ = False

blocks = length . filter isBlock . fmap snd . Map.toList

solve1 :: IO ()
solve1 = do
  program <- input "input/day13.txt"
  let tiles = blocks . finish [] $ ((0, 0), program)
  print tiles

-- Question 2

yield :: [Integer] -> Integer -> Computer -> Either Integer (Either Integer Integer, Computer)
yield input = go
  where
    go score state = case continues 3 input state of
      ([-1, 0, score'], state') -> go score' state'
      ([x, y, memTile], state') ->
        let tile = fromMemory memTile
         in case tile of
              Ball -> Right (Left x, state')
              Paddle -> Right (Right x, state')
              _ -> go score state'
      (_, state') -> Left score

sign :: (Ord a, Eq a, Num a) => a -> a
sign 0 = 0
sign x = if x < 0 then -1 else 1

loop :: Integer -> (Integer, Integer) -> Computer -> IO Integer
loop score (ball, paddle) computer = do
  let dir = sign (ball - paddle)
  case yield [dir] score computer of
    Right (o, computer') ->
      loop score (either (,paddle) (ball,) o) computer'
    Left score -> return score

solve2 :: IO ()
solve2 = do
  program <- (memory . (2 :) . tail . fmap read . splitOn "," <$>) . readFile $ "input/day13.txt"
  result <- loop 0 (0, 0) ((0, 0), program)
  print result
