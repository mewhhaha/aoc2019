{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Solution.Day15 where

import Computer
import Control.Arrow
import Control.Monad.Loops
import qualified Data.Map as Map
import qualified Data.Set as Set
import Polysemy

data Direction = N | S | W | E
  deriving (Enum)

data Report = Unchanged | At | Target
  deriving (Enum)

data Tile = Unexplored | Explored | Wall

data Graph m a where
  Explore :: (Integer, Integer) -> Graph m a
  Define :: (Integer, Integer) -> Tile -> Graph m a
  Next :: Graph m (Maybe (Integer, Integer))
  Path :: (Integer, Integer) -> (Integer, Integer) -> Graph m [Direction]

data Environment m a where
  Move :: [Direction] -> Environment m Report
  SetEnd :: (Integer, Integer) -> Environment m ()
  GetEnd :: Environment m (Integer, Integer)

makeSem ''Environment

makeSem ''Graph

instance Mem Direction where

  toMemory = toInteger . (+ 1) . fromEnum

  fromMemory = toEnum . (+ 1) . fromInteger

instance Mem Report where

  toMemory = toInteger . fromEnum

  fromMemory = toEnum . fromInteger

back :: Direction -> (Integer, Integer) -> (Integer, Integer)
back N = second pred
back S = second succ
back E = first pred
back W = first succ

pathToSystem :: Members [Graph, Environment] r => (Integer, Integer) -> Sem r [Direction]
pathToSystem start = go start
  where
    go :: Members [Graph, Environment] r => (Integer, Integer) -> Sem r [Direction]
    go pos = next >>= \case
      Nothing -> do
        end <- getEnd
        path start end
      Just dest -> do
        directions <- path pos dest
        let continue = explore dest >> go dest
        move directions >>= \case
          Unchanged -> do
            define dest Wall
            go (back (last directions) dest)
          At -> continue
          Target -> setEnd dest >> continue

findPath :: (Integer, Integer) -> (Integer, Integer) -> ((Integer, Integer) -> [(Integer, Integer)]) -> [(Integer, Integer)]
findPath start end getNeighbours = go (0, start) mempty (Map.singleton start 0)
  where
    trace visited curr
      | curr == start = [start]
      | otherwise = (trace visited fst . minimum snd $ flip (Map.findWithDefault maxBound) visited <$> (visited Map.! curr)) ++ [curr]
    go (t, currNode) unvisited visited
      | currNode == end = trace visited end
      | otherwise =
        let nodes = filter (`Map.member` visited) $ getNeighbours currNode
            visited' = Map.insert currNode t visited
            (nextNode, unvisited') = Set.deleteFindMin . foldr (Set.insert . (t + 1,)) unvisited $ nodes
         in go nextNode unvisited' visited'
