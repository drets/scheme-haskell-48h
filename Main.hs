{-# LANGUAGE OverloadedStrings #-}
module Main where

import System.Environment
import Text.ParserCombinators.Parsec hiding (spaces)
import Text.Parsec.Prim
import Control.Monad
import Numeric
import Data.Foldable
import Data.Char


symbol :: Parser Char
symbol = oneOf "!#$%&|*+-/:<=>?@^_~"

readExpr :: String -> String
readExpr input = case parse parseExpr "list" input of
  Left err -> "No match: " ++ show err
  Right val -> "Found value:\n" ++ show val

spaces :: Parser ()
spaces = skipMany1 space

main :: IO ()
main = do
  (expr:_) <- getArgs
  putStrLn (readExpr expr)

data LispValue = Atom String
               | List [LispValue]
               | DottedList [LispValue] LispValue
               | NumberBase [(Integer, String)]
               | Number Integer
               | Character String
               | String String
               | Bool Bool
               deriving (Show)

parseString :: Parser LispValue
parseString = do
  char '"'
  x <- many1 ((char '\\' >> char '"')
              <|> newline
              <|> tab
              <|> (char '\\' >> char 'r')
              <|> (char '\\' >> char '\\')
              <|> noneOf "\"")
  char '"'
  return $ String x

parseAtom :: Parser LispValue
parseAtom = do
  first <- letter <|> symbol
  rest <- many (letter <|> digit <|> symbol)
  let atom = first:rest
  return $ case atom of
    "#t" -> Bool True
    "#f" -> Bool False
    _    -> Atom atom

parseNumber :: Parser LispValue
parseNumber = parseInt <|> (char '#' >> (parseDecimal <|> parseOctal <|> parseHex <|> parseBinary))

parseBinary :: Parser LispValue
parseBinary = do
  char 'b'
  x <- many1 (oneOf "01")
  return $ Number (fromIntegral $ binToDec x)

binToDec :: String -> Int
binToDec = foldl' (\acc x -> acc * 2 + digitToInt x) 0

parseInt :: Parser LispValue
parseInt = do
  x <- many1 digit
  return $ Number (read x)

parseOctal :: Parser LispValue
parseOctal = do
  char 'o'
  x <- many1 octDigit
  let first:_ = readOct x
  return $ Number (fst first)

parseHex :: Parser LispValue
parseHex = do
  char 'x'
  x <- many1 hexDigit
  let first:_ = readHex x
  return $ Number (fst first)

parseDecimal :: Parser LispValue
parseDecimal = do
  char 'd'
  x <- many1 digit
  let first:_ = readDec x
  return $ Number (fst first)

parseExpr :: Parser LispValue
parseExpr = parseCharacter <|> parseString <|> parseNumber <|> parseAtom

parseCharacter :: Parser LispValue
parseCharacter = do
  char '#'
  char '\\'
  first <- anyChar
  rest <- many (noneOf " ")
  return $ Character (first:rest)
