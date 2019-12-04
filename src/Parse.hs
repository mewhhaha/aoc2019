{-# LANGUAGE TypeApplications #-}

module Parse where

import Data.List
import Data.Monoid

splitOn :: String -> String -> [String]
splitOn = go []
  where
    go buffer t [] = []
    go buffer t s@(x : xs) = if match then reverse buffer : go buffer t rest else go (x : buffer) t xs
      where
        match = foldl1 (&&) $ zipWith (==) t s
        rest = drop (length t) s
