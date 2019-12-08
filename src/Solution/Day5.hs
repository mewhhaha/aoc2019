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
import Polysemy
import Polysemy.NonDet
import Polysemy.State
import Polysemy.Writer
import qualified Test

data Variable = Value | Pointer

data Opcode = Plus | Mul | Input | Output | LessThan | Equals | IfTrueJump | IfFalseJump | Terminate
  deriving (Eq)

data Program m a where
  Spit :: Int -> Program m ()
  Swallow :: Int -> Program m ()
  Memorize :: Int -> Int -> Program m ()
  Consume :: Variable -> Program m Int
  Jump :: Int -> Program m ()

makeSem ''Program

type Memory = Map.Map Int Int

memory :: [Int] -> Memory
memory = Map.fromList . zip [0 ..]

input :: IO Memory
input = memory . fmap read . splitOn "," <$> readFile "input/day5.txt"

-- Question 1

parseOpcode :: Int -> Opcode
parseOpcode = toOpcode . read . reverse . take 2 . reverse . show
  where
    toOpcode = \case
      99 -> Terminate
      x -> [Plus, Mul, Input, Output, IfTrueJump, IfFalseJump, LessThan, Equals] !! (x - 1)

parseVariables :: Int -> [Variable]
parseVariables = toVariables . trailingZeros . dropOp . leftToRight . toDigits
  where
    toDigits = show
    leftToRight = reverse
    dropOp = drop 2
    trailingZeros = (++ repeat '0')
    toVariables = fmap toVariable
    toVariable = \case
      '0' -> Pointer
      '1' -> Value

consumes :: Member Program r => [Variable] -> Sem r [Int]
consumes = mapM consume

save :: Member Program r => Int -> Sem r ()
save v = do
  m <- consume Value
  memorize m v

program :: Members [Program, NonDet] r => Sem r ()
program = do
  (vars, opcode) <- (parseVariables &&& parseOpcode) <$> consume Value
  case opcode of
    Terminate -> pure ()
    x -> do
      case x of
        Plus -> do
          [x, y] <- consumes $ take 2 vars
          save (x + y)
        Mul -> do
          [x, y] <- consumes $ take 2 vars
          save (x * y)
        Input -> do
          m <- consume Value
          swallow m
        Output -> do
          [x] <- consumes $ take 1 vars
          spit x
        IfTrueJump -> do
          [x, to] <- consumes $ take 2 vars
          if x /= 0 then jump to else pure ()
        IfFalseJump -> do
          [x, to] <- consumes $ take 2 vars
          if x == 0 then jump to else pure ()
        LessThan -> do
          [x, y] <- consumes $ take 2 vars
          save (if x < y then 1 else 0)
        Equals -> do
          [x, y] <- consumes $ take 2 vars
          save (if x == y then 1 else 0)
      program

step :: Member (State Int) r => Sem r Int
step = do
  x <- get
  put (x + 1)
  pure x

goto :: Member (State Int) r => Int -> Sem r ()
goto = put

store :: Member (State Memory) r => Int -> Int -> Sem r ()
store = (modify .) . Map.insert

from :: Member (State Memory) r => Int -> Sem r Int
from = gets . flip (Map.!)

record :: Member (Writer [Int]) r => Int -> Sem r ()
record = tell . (: [])

runProgram :: Members [State Int, State Memory, Writer [Int]] r => Int -> Sem (Program : r) a -> Sem r a
runProgram id = interpret $ \case
  Spit x -> record x
  Jump m -> goto m
  Swallow x -> store x id
  Memorize m x -> store m x
  Consume var -> step
    >>= from
    >>= case var of
      Value -> pure
      Pointer -> from

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
  print result

-- Question 2

solve2 :: IO ()
solve2 = do
  mem <- input
  let result = runAll mem 5
  print result
