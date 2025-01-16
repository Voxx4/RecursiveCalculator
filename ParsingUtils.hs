module ParsingUtils where

import DataTypes
import Text.Megaparsec
import Text.Megaparsec.Char
import Data.Void
import Data.Char
import Data.Either
  
type Parser = Parsec Void String

parseName :: Parser String
parseName = do
  funcName <- some $ satisfy isAlphaNum
  _ <- space
  return funcName

parseNum :: Parser String
parseNum = do 
  num <- some numberChar
  return num

parseLine :: Parser LineAST
parseLine = do
  funcName <- parseName 
  _ <- space
  next <- printChar
  case next of
    '=' -> do
      space
      try parseMin <|> parseSup
      where 
        parseMin = do
          _ <- string "n["
          h <- parseName
          _ <- char ']'
          _ <- space
          return $ MinimisationLine funcName h
        parseSup = do
          g <- parseName
          _ <- char '('
          hs <- some (do
            space
            h <- parseName
            return h)
          _ <- space
          _ <- char ')'
          _ <- space
          return $ SuperpositionLine funcName g hs
    '0' -> do
      space
      char '='
      space
      g <- parseName
      _ <- space
      return $ PrimitiveRecursionBase funcName g
    'n' -> do
      char '+'
      char '1'
      space
      char '='
      space
      h <- parseName
      _ <- space
      return $ PrimitiveRecursionStep funcName h
    _ -> fail "Unknown function type"
  --arguments <- parseArgs

parseFile :: Parser [LineAST]
parseFile = do 
  lines <- some (do 
    line <- parseLine
    --eol
    return line)
  _ <- eof
  return lines