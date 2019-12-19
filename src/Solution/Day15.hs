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
import Control.Monad
import Control.Monad.Loops
import Data.List
import qualified Data.Map as Map
import Data.Maybe
import qualified Data.Set as Set
import Debug.Trace
import Polysemy
import Polysemy.State

data Direction = N | S | W | E
  deriving (Enum, Show)

data Report = Unchanged | At | Target
  deriving (Enum)

data Tile = Explored | Wall | X

type Node = (Integer, Integer)

data Carta m a where
  Visit :: Tile -> Carta m ()
  Next :: Carta m (Maybe Node)
  End :: Carta m (Maybe Node)
  Path :: Node -> Node -> Carta m [Direction]

data Action m a where
  Move :: [Direction] -> Action m Report

makeSem ''Action

makeSem ''Carta

instance Mem Direction where

  toMemory = toInteger . (+ 1) . fromEnum

  fromMemory = toEnum . (+ 1) . fromInteger

instance Mem Report where

  toMemory = toInteger . fromEnum

  fromMemory = toEnum . fromInteger

back :: Direction -> Node -> Node
back N = second pred
back S = second succ
back E = first pred
back W = first succ

pathfinder :: Members [Carta, Action] r => Sem r [Direction]
pathfinder = go start
  where
    start = (0, 0)
    go :: Members [Carta, Action] r => Node -> Sem r [Direction]
    go pos = next >>= \case
      Nothing -> do
        x <- fromJust <$> end
        path start x
      Just dest -> do
        directions <- path pos dest
        move directions >>= \case
          Unchanged -> do
            visit Wall
            go (back (last directions) dest)
          At -> visit Explored >> go dest
          Target -> visit X >> go dest

findPath :: Node -> Node -> (Node -> [Node]) -> [Node]
findPath start end getNeighbours = go (0, start) mempty (Map.singleton start 0)
  where
    go (t, currNode) unvisited visited
      | currNode == end = reverse $ backtrack end
      | otherwise =
        let visited' = Map.insert currNode t visited
            nodes = filter (`Map.notMember` visited) . getNeighbours $ currNode
            (nextNode, unvisited') = Set.deleteFindMin . foldr (Set.insert . (t + 1,)) unvisited $ nodes
         in go nextNode unvisited' visited'
      where
        backtrack curr
          | curr == start = [start]
          | otherwise =
            let origin node = (Map.findWithDefault 9999 node visited, node)
             in curr : (backtrack . snd . minimum $ origin <$> getNeighbours curr)

calcNeighbours :: (Integer, Integer) -> [(Integer, Integer)]
calcNeighbours (x, y) = [(x + 1, y), (x - 1, y), (x, y + 1), (x, y - 1)]

connect :: Set.Set Node -> (Node -> [Node])
connect graph = filter (`Set.member` graph) . calcNeighbours

isWall :: Tile -> Bool
isWall Wall = True
isWall _ = False

isX :: Tile -> Bool
isX X = True
isX _ = False

data Graph = Graph {getEnd :: Maybe Node, getUnexplored :: [Node], getExplored :: Set.Set Node}
  deriving (Show)

toDirections :: [Node] -> [Direction]
toDirections ns = zipWith toDirection ns (tail ns)
  where
    toDirection (x, y) (x', y')
      | y' > y = N
      | y' < y = S
      | x' > x = E
      | x' < x = W

paint :: (Integer, Integer) -> Set.Set Node -> String
paint (w, h) set = intercalate "\n" $ fmap (fmap panel) painting
  where
    w½ = w `div` 2
    h½ = h `div` 2
    panel pos = if Set.member pos set then '#' else '.'
    painting = fmap ((<$> [- h½ .. h½]) . flip (,)) [w½, w½ -1 .. - w½]

runCarta :: Member (State Graph) r => Sem (Carta : r) a -> Sem r a
runCarta = interpret $ \case
  Visit t ->
    if isWall t
      then modify (\(Graph a b c) -> Graph a (tail b) c)
      else do
        Graph goal unexplored explored <- get
        let (node : rest) = unexplored
            neighbours = calcNeighbours node
            unexplored' = filter (`Set.notMember` explored) neighbours ++ rest
            graph' = Set.insert node explored
            goal' = if isX t then Just node else goal
        put $ Graph goal' unexplored' graph'
  Next -> gets (listToMaybe . getUnexplored)
  End -> gets getEnd
  Path from to -> toDirections . findPath from to <$> gets (connect . Set.insert to . getExplored)

runAction :: Member (State Computer) r => Sem (Action : r) a -> Sem r a
runAction = interpret $ \case
  Move dirs -> do
    computer <- get
    let (Just (out, computer')) = foldM (\(_, comp) d -> continue comp [d]) (-1, computer) (toMemory <$> dirs)
    put computer'
    return $ fromMemory out

runPaint :: Member (State Graph) r => Sem r a -> Sem r a
runPaint = intercept $ \case
  Put g@(Graph _ _ m) -> trace (paint (50, 50) m) (put g)
  Get -> get

runRobot program =
  run
    . runState (Graph Nothing unexplored (Set.singleton start))
    -- . runPaint
    . runCarta
    . runState @Computer ((0, 0), program)
    . runAction
    $ pathfinder
  where
    start = (0, 0)
    unexplored = [(1, 0), (-1, 0), (0, 1), (0, -1)]

solve1 :: IO ()
solve1 = do
  program <- input "input/day15.txt"
  let (_, (_, p)) = runRobot program
  print $ length p

-- Question 2

maxDepth :: Set.Set Node -> Node -> Integer
maxDepth e node = case filter (`Set.member` e) . calcNeighbours $ node of
  [] -> 0
  xs -> 1 + maximum (maxDepth (Set.delete node e) <$> xs)

solve2 :: IO ()
solve2 = do
  program <- input "input/day15.txt"
  let (Graph (Just o) _ e, _) = runRobot program
      minutes = maxDepth e o
  print minutes
