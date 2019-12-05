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

import Data.List
import Data.List.Split
import qualified Data.Map as Map
import Debug.Trace
import Polysemy
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

process :: Member Program r => [Int] -> Sem r [Int]
process = mapM go
  where
    go = \case
      0 -> consume >>= remember
      1 -> consume

opcode :: Int -> Int
opcode = read . reverse . take 2 . reverse . show

modes :: Int -> [Int]
modes = toInts . groupNegative . trailingZeros . dropOp . toDigits
  where
    toDigits = reverse . show
    dropOp = drop 2
    trailingZeros = (++ repeat '0')
    groupNegative = groupBy (\_ y -> y == '-')
    toInts = fmap (read . reverse)

binop :: Member Program r => Int -> (Int -> Int -> Int) -> Sem r ()
binop q op = process (take 2 $ modes q) >>= \[x, y] -> consume >>= \m -> memorize m (x `op` y)

feed :: Member Program r => Int -> Int -> Sem r [Int]
feed n q = process (take n $ modes q)

program :: Member Program r => Sem r ()
program = do
  let continue = program
  q <- consume
  if q == 99
    then die
    else do
      case opcode q of
        1 -> binop q (+)
        2 -> binop q (*)
        3 -> consume >>= swallow
        4 -> feed 1 q >>= (spit . head)
        5 -> feed 2 q >>= \[x, m] -> yes x m
        6 -> feed 2 q >>= \[x, m] -> no x m
        7 -> binop q ((fromEnum .) . (<))
        8 -> binop q ((fromEnum .) . (==))
      continue

next :: Member (State Int) r => Sem r Int
next = do
  x <- get
  modify @Int (+ 1)
  pure x

runProgram :: Members [State Int, State Memory, Writer [Int]] r => Int -> Sem (Program : r) a -> Sem r a
runProgram id = interpret $ \case
  Spit x -> tell @[Int] [x]
  Yes x m -> if x /= 0 then put @Int m else pure ()
  No x m -> if x == 0 then put @Int m else pure ()
  Swallow x -> modify @Memory (Map.insert x id)
  Memorize m x -> modify @Memory (Map.insert m x)
  Remember m -> gets @Memory (Map.! m)
  Consume -> next >>= \m -> gets @Memory (Map.! m)
  Die -> pure ()

runAll :: Memory -> Int -> [Int]
runAll mem id =
  fst
    . run
    . runWriter @[Int]
    . runState @Int 0
    . runState @Memory mem
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
