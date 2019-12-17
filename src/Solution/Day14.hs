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

module Solution.Day14 where

-- import Data.List.Split

import Data.Either
import qualified Data.Map as Map
import Data.Maybe
import Data.Void
import Debug.Trace
import Polysemy
import Polysemy.Reader
import Polysemy.State
import qualified Test
import Text.Megaparsec (Parsec, errorBundlePretty, many, parse, sepBy)
import Text.Megaparsec.Char (digitChar, newline, space1, string, upperChar)

data Element = Element {getAmount :: Integer, getName :: String}
  deriving (Show)

data Reaction = Reaction [Element] Element
  deriving (Show)

type Parser = Parsec Void String

data Backpack m a where
  Stash :: Element -> Backpack m ()
  Unstash :: String -> Backpack m Integer

data Cookbook m a where
  Recipe :: String -> Cookbook m (Integer -> (Integer, [Element]))

makeSem ''Backpack

makeSem ''Cookbook

parseElement :: Parser Element
parseElement = do
  i <- read <$> many digitChar
  space1
  Element i <$> many upperChar

parseRequirements :: Parser [Element]
parseRequirements = parseElement `sepBy` string ", "

parseResult :: Parser Element
parseResult = parseElement

parseReaction :: Parser Reaction
parseReaction = do
  reqs <- parseRequirements
  string " => "
  Reaction reqs <$> parseResult

parseReactions :: Parser [Reaction]
parseReactions = parseReaction `sepBy` newline

multiply :: Integer -> Element -> Element
multiply x (Element n name) = Element (x * n) name

sign :: Integer -> Integer
sign 0 = 0
sign x = if x > 0 then 1 else -1

leftovers :: Member Backpack r => Integer -> String -> Sem r Integer
leftovers n name = do
  stashed <- unstash name
  let stashed' = max (stashed - n) 0
      n' = max (n - stashed) 0
  stash $ Element stashed' name
  return n'

exhaust :: Members [Backpack, Cookbook] r => Sem r Integer
exhaust = go (Element 1 "FUEL")
  where
    go :: Members [Backpack, Cookbook] r => Element -> Sem r Integer
    go (Element 0 _) = return 0
    go (Element n "ORE") = return n
    go (Element n name) = do
      needed <- leftovers n name
      cook <- recipe name
      let (cooked, used) = cook needed
      stash (Element (cooked - needed) name)
      sum <$> mapM go used

runBackpack :: Member (State (Map.Map String Integer)) r => Sem (Backpack : r) a -> Sem r a
runBackpack = interpret $ \case
  Stash (Element n name) -> modify (Map.insertWith (+) name n)
  Unstash name -> do
    (value, backpack) <- gets $ Map.updateLookupWithKey (\_ _ -> Nothing) name
    put backpack
    return $ fromMaybe 0 value

runCookbook :: Member (Reader (Map.Map String (Integer -> (Integer, [Element])))) r => Sem (Cookbook : r) a -> Sem r a
runCookbook = interpret $ \case
  Recipe name -> asks (Map.! name)

runAll cookbook =
  snd
    . run
    . runReader cookbook
    . runState mempty
    . runBackpack
    . runCookbook

input :: IO [Reaction]
input = do
  let file = "input/day14.txt"
  content <- readFile file
  case parse parseReactions file content of
    Left bundle -> putStr (errorBundlePretty bundle) >> return []
    Right rs -> return rs

toCookbook :: [Reaction] -> Map.Map String (Integer -> (Integer, [Element]))
toCookbook = foldr (\(Reaction reqs (Element n name)) -> Map.insert name (recipe reqs n)) mempty
  where
    recipe reqs n x =
      let (d, m) = divMod x n
          overflow = sign m
          factor = d + overflow
       in (factor * n, fmap (multiply factor) reqs)

day14examples =
  [ ( [ Reaction [Element 9 "ORE"] (Element 2 "A"),
        Reaction [Element 8 "ORE"] (Element 3 "B"),
        Reaction [Element 7 "ORE"] (Element 5 "C"),
        Reaction [Element 3 "A", Element 4 "B"] (Element 1 "AB"),
        Reaction [Element 5 "B", Element 7 "C"] (Element 1 "BC"),
        Reaction [Element 4 "C", Element 1 "A"] (Element 1 "CA"),
        Reaction [Element 2 "AB", Element 3 "BC", Element 4 "CA"] (Element 1 "FUEL")
      ],
      165
    )
  ]

test1 :: IO ()
test1 = Test.run (flip runAll exhaust . toCookbook) day14examples

solve1 :: IO ()
solve1 = do
  cookbook <- toCookbook <$> input
  let result = runAll cookbook exhaust
  print result
