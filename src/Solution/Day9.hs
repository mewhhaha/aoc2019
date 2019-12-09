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

module Solution.Day9 where

import Control.Arrow
import Control.Monad
import Data.List
import Data.List.Split
import qualified Data.Map as Map
import Data.Maybe
import Debug.Trace
import Polysemy
import Polysemy.NonDet
import Polysemy.State
import Polysemy.Writer
import qualified Test

data Variable = Immediate | Position | Relative
  deriving (Show)

data Opcode = Plus | Mul | Input | Yield | LessThan | Equals | IfTrueJump | IfFalseJump | Terminate | AdjustRelative
  deriving (Eq, Show)

data Program m a where
  Spit :: Integer -> Program m ()
  Swallow :: Integer -> Program m ()
  Memorize :: Integer -> Integer -> Program m ()
  Consume :: Variable -> Program m Integer
  Jump :: Integer -> Program m ()
  Relativise :: Integer -> Program m ()

makeSem ''Program

type Memory = Map.Map Integer Integer

memory :: [Integer] -> Memory
memory = Map.fromList . zip [0 ..]

input :: IO Memory
input = memory . fmap read . splitOn "," <$> readFile "input/day9.txt"

-- Question 1

parseOpcode :: Integer -> Opcode
parseOpcode = toOpcode . read . reverse . take 2 . reverse . show
  where
    toOpcode = \case
      99 -> Terminate
      x -> [Plus, Mul, Input, Yield, IfTrueJump, IfFalseJump, LessThan, Equals, AdjustRelative] !! (x - 1)

parseVariables :: Integer -> [Variable]
parseVariables = toVariables . trailingZeros . dropOp . leftToRight . toDigits
  where
    toDigits = show
    leftToRight = reverse
    dropOp = drop 2
    trailingZeros = (++ repeat '0')
    toVariables = fmap $ \case
      '0' -> Position
      '1' -> Immediate
      '2' -> Relative

consumes :: Member Program r => [Variable] -> Sem r [Integer]
consumes = mapM consume

save :: Member Program r => Integer -> Sem r ()
save v = do
  m <- consume Immediate
  memorize m v

program :: Members [Program, NonDet] r => Sem r ()
program = do
  (vars, opcode) <- (parseVariables &&& parseOpcode) <$> consume Immediate
  case opcode of
    Terminate -> pure ()
    Yield -> consumes (take 1 vars) >>= spit . head
    x -> do
      case x of
        Plus -> consumes (take 2 vars) >>= save . sum
        Mul -> consumes (take 2 vars) >>= save . product
        Input -> consume Immediate >>= swallow
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
        AdjustRelative -> consumes (take 1 vars) >>= relativise . head
      program

step :: Member (State ((Integer, Integer), Memory)) r => Sem r Integer
step = do
  ((p, r), m) <- get
  put ((p + 1, r), m)
  pure p

goto :: Member (State ((Integer, Integer), Memory)) r => Integer -> Sem r ()
goto = modify . (first . first) . const

store :: Member (State ((Integer, Integer), Memory)) r => Integer -> Integer -> Sem r ()
store p v = modify @((Integer, Integer), Memory) (Map.insert p v <$>)

from :: Member (State ((Integer, Integer), Memory)) r => Integer -> Sem r Integer
from = gets . (. snd) . flip (Map.!)

record :: Member (Writer [Integer]) r => Integer -> Sem r ()
record = tell . (: [])

receive :: Member (State [Integer]) r => Sem r Integer
receive = do
  inputs <- get
  case inputs of
    [] -> undefined
    (x : xs) -> put xs >> pure x

offset :: Member (State ((Integer, Integer), Memory)) r => Integer -> Sem r ()
offset = modify . (first . second) . (+)

relativeBase :: Member (State ((Integer, Integer), Memory)) r => Sem r Integer
relativeBase = gets (snd . fst)

runProgram :: Members [State [Integer], State ((Integer, Integer), Memory), Writer [Integer]] r => Sem (Program : r) a -> Sem r a
runProgram = interpret $ \case
  Spit x -> record x
  Jump p -> goto p
  Swallow x -> receive >>= store x
  Memorize p x -> store p x
  Consume var -> step
    >>= from
    >>= case var of
      Immediate -> pure
      Position -> from
      Relative -> \p -> do
        r <- relativeBase
        from (r + p)
  Relativise r -> offset r

runAll :: ((Integer, Integer), Memory) -> [Integer] -> Maybe (Integer, ((Integer, Integer), Memory))
runAll computer input =
  let (result, (state, _)) =
        run
          . runWriter
          . runState computer
          . runState input
          . runNonDet @IO
          . runProgram
          $ program
   in if null result
        then Nothing
        else Just (head result, state)

runRepeat mem =
  iterate
    ( \case
        Nothing -> Nothing
        Just (_, state) -> runAll state []
    )
    (runAll ((0, 0), mem) [1])

solve1 :: IO ()
solve1 = do
  mem <- input
  let result = fmap fst . catMaybes . takeWhile isJust $ runRepeat mem
  print result
