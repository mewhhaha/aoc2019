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

module Computer
  ( runYield,
    exec,
    input,
    memory,
  )
where

import Control.Arrow
import Data.Char
import Data.List
import Data.List.Split
import qualified Data.Map as Map
import Data.Maybe
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

relative :: Member (State ((Integer, Integer), Memory)) r => (Integer -> Sem r a) -> Integer -> Sem r a
relative f p = gets (snd . fst) >>= f . (+ p)

runProgram :: Members [State [Integer], State ((Integer, Integer), Memory), Writer [Integer]] r => Sem (Program : r) a -> Sem r a
runProgram = interpret $ \case
  Spit x -> record x
  Jump p -> goto p
  Swallow -> receive
  Memorize mode p x -> case mode of
    Position -> store p x
    Relative -> relative (`store` x) p
  Consume mode -> step
    >>= from
    >>= case mode of
      Immediate -> pure
      Position -> from
      Relative -> relative from
  Rethink r -> offset r

runYield :: ((Integer, Integer), Memory) -> [Integer] -> Maybe (Integer, ((Integer, Integer), Memory))
runYield computer input =
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

exec :: [Integer] -> Memory -> [Integer]
exec input mem =
  fmap fst . catMaybes . takeWhile isJust $
    iterate
      (>>= (`runYield` []) . snd)
      (runYield ((0, 0), mem) input)
