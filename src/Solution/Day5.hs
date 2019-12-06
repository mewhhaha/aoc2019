{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Solution.Day5 where

import Control.Arrow
import Data.List
import Data.List.Split
import qualified Data.Map as Map
import Debug.Trace
import Polysemy
import Polysemy.NonDet
import Polysemy.State
import Polysemy.Writer
import qualified Test

data Program m a where
  Spit :: Int -> Program m ()
  Swallow :: Int -> Program m ()
  Memorize :: Int -> Int -> Program m ()
  Remember :: Int -> Program m Int
  Consume :: Program m Int
  Yes :: Int -> Int -> Program m ()
  No :: Int -> Int -> Program m ()
  Die :: Program m ()

makeSem ''Program

type Memory = Map.Map Int Int

memory :: [Int] -> Memory
memory = Map.fromList . zip [0 ..]

input :: IO Memory
input = memory . fmap read . splitOn "," <$> readFile "input/day5.txt"

-- Question 1

opcode :: Int -> Int
opcode = read . reverse . take 2 . reverse . show

parameters :: Int -> [Int]
parameters = toInts . groupNegative . trailingZeros . dropOp . toDigits
  where
    toDigits = reverse . show
    dropOp = drop 2
    trailingZeros = (++ repeat '0')
    groupNegative = groupBy (\_ y -> y == '-')
    toInts = fmap (read . reverse)

modes :: Member Program r => [Int] -> Sem r [Int]
modes = mapM go
  where
    go = \case
      0 -> consume >>= remember
      1 -> consume

feed :: Member Program r => Int -> [Int] -> Sem r [Int]
feed n params = modes (take n $ params)

save :: Member Program r => Int -> Sem r ()
save v = do
  m <- consume
  memorize m v

program :: Members [Program, NonDet] r => Sem r ()
program = do
  let continue = program
  (params, op) <- (parameters &&& opcode) <$> consume
  if op == 99
    then die
    else do
      case op of
        1 -> do
          [x, y] <- feed 2 params
          save (x + y)
        2 -> do
          [x, y] <- feed 2 params
          save (x * y)
        3 -> do
          m <- consume
          swallow m
        4 -> do
          [x] <- feed 1 params
          spit x
        5 -> do
          [x, to] <- feed 2 params
          yes x to
        6 -> do
          [x, to] <- feed 2 params
          no x to
        7 -> do
          [x, y] <- feed 2 params
          save (if x < y then 1 else 0)
        8 -> do
          [x, y] <- feed 2 params
          save (if x == y then 1 else 0)
      continue

pop :: Member (State Int) r => Sem r Int
pop = do
  x <- get
  modify @Int (+ 1)
  pure x

jump :: Member (State Int) r => Int -> Sem r ()
jump = put

store :: Member (State Memory) r => Int -> Int -> Sem r ()
store = (modify .) . Map.insert

view :: Member (State Memory) r => Int -> Sem r Int
view = gets . flip (Map.!)

record :: Member (Writer [Int]) r => Int -> Sem r ()
record = tell . (: [])

runProgram :: Members [State Int, State Memory, Writer [Int]] r => Int -> Sem (Program : r) a -> Sem r a
runProgram id = interpret $ \case
  Spit x -> record x
  Yes x m -> if x /= 0 then jump m else pure ()
  No x m -> if x == 0 then jump m else pure ()
  Swallow x -> store x id
  Memorize m x -> store m x
  Remember m -> view m
  Consume -> pop >>= view
  Die -> pure ()

runAll :: Memory -> Int -> [Int]
runAll mem id =
  fst
    . run
    . runWriter @[Int]
    . runState @Int 0
    . runState @Memory mem
    . runNonDet @IO
    . runProgram id
    $ program

solve1 :: IO ()
solve1 = do
  mem <- input
  let result = runAll mem 1
  putStrLn $ show result

-- Question 2

solve2 :: IO ()
solve2 = do
  mem <- input
  let result = runAll mem 5
  putStrLn $ show result
