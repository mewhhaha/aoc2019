module Solution.Day14 where

-- import Data.List.Split

import Data.Either
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

data Element = Element Int String
  deriving (Show)

data Reaction = Reaction [Element] Element
  deriving (Show)

type Parser = Parsec Void String

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

input :: IO [Reaction]
input = do
  let file = "input/day14.txt"
  content <- readFile file
  case parse parseReactions file content of
    Left bundle -> putStr (errorBundlePretty bundle) >> return []
    Right rs -> return rs

solve1 :: IO ()
solve1 = do
  reactions <- input
  print reactions
