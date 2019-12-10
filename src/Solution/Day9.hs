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
import Data.Char
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

data Mode = Position | Immediate | Relative
  deriving (Show, Enum)

data Opcode = Terminate | Plus | Mul | Input | Yield | IfTrueJump | IfFalseJump | LessThan | Equals | AdjustRelative
  deriving (Eq, Show, Enum)

data Program m a where
  Spit :: Integer -> Program m ()
  Swallow :: Program m Integer
  Memorize :: Mode -> Integer -> Integer -> Program m ()
  Consume :: Mode -> Program m Integer
  Jump :: Integer -> Program m ()
  Rethink :: Integer -> Program m ()

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
      x -> toEnum x

parseModes :: Integer -> [Mode]
parseModes = toModes . trailingZeros . dropOp . leftToRight . toDigits
  where
    toDigits = show
    leftToRight = reverse
    dropOp = drop 2
    trailingZeros = (++ repeat '0')
    toModes = fmap $ toEnum . digitToInt

consumes :: Member Program r => [Mode] -> Sem r [Integer]
consumes = mapM consume

save :: Member Program r => Mode -> Integer -> Sem r ()
save mode v = do
  m <- consume Immediate
  memorize mode m v

program :: Members [Program, NonDet] r => Sem r ()
program = do
  (modes, opcode) <- (parseModes &&& parseOpcode) <$> consume Immediate
  let mode x = (!! max 0 (x -1)) modes
  case opcode of
    Terminate -> pure ()
    Yield -> consumes (take 1 modes) >>= spit . head
    x -> do
      case x of
        Plus -> consumes (take 2 modes) >>= save (mode 3) . sum
        Mul -> consumes (take 2 modes) >>= save (mode 3) . product
        Input -> swallow >>= save (mode 1)
        IfTrueJump -> do
          [x, to] <- consumes $ take 2 modes
          if x /= 0 then jump to else pure ()
        IfFalseJump -> do
          [x, to] <- consumes $ take 2 modes
          if x == 0 then jump to else pure ()
        LessThan -> do
          [x, y] <- consumes $ take 2 modes
          save (mode 3) (if x < y then 1 else 0)
        Equals -> do
          [x, y] <- consumes $ take 2 modes
          save (mode 3) (if x == y then 1 else 0)
        AdjustRelative -> consume (mode 1) >>= rethink
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
from p = gets (fromMaybe 0 . Map.lookup p . snd)

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
  Swallow -> receive
  Memorize mode p x -> case mode of
    Position -> store p x
    Relative -> do
      r <- relativeBase
      store (r + p) x
  Consume mode -> step
    >>= from
    >>= \v -> case mode of
      Immediate -> pure v
      Position -> from v
      Relative -> do
        r <- relativeBase
        from (r + v)
  Rethink r -> offset r

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

day9examples =
  [ (memory [109, 1, 204, -1, 1001, 100, 1, 100, 1008, 100, 16, 101, 1006, 101, 0, 99], [109, 1, 204, -1, 1001, 100, 1, 100, 1008, 100, 16, 101, 1006, 101, 0, 99]),
    (memory [1102, 34915192, 34915192, 7, 4, 7, 99, 0], [1219070632396864]),
    (memory [104, 1125899906842624, 99], [1125899906842624])
  ]

exec :: [Integer] -> Memory -> [Integer]
exec input mem =
  fmap fst . catMaybes . takeWhile isJust $
    iterate
      (>>= (`runAll` []) . snd)
      (runAll ((0, 0), mem) input)

test1 :: IO ()
test1 = Test.run (exec []) day9examples

solve1 :: IO ()
solve1 = do
  mem <- input
  let result = exec [1] mem
  print result

-- Question 2

solve2 :: IO ()
solve2 = do
  mem <- input
  let result = exec [2] mem
  print result
