{-# LANGUAGE OverloadedStrings #-}
module Main where

import System.Environment
import Text.ParserCombinators.Parsec hiding (spaces, try)
import Text.Parsec.Prim
import Control.Monad
import Numeric
import Data.Foldable
import Data.Char

symbol :: Parser Char
symbol = oneOf "!#$%&|*+-/:<=>?@^_~"

readExpr :: String -> String
readExpr input = case parse parseExpr "scheme" input of
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
               | RealNumber LispReal
               | RationalNumber LispRational
               | ComplexNumber LispComplex
               | Character String
               | String String
               | Bool Bool
               deriving (Eq, Show)

data LispComplex =
  LispComplex LispReal LispReal
  deriving (Eq, Show)

data LispReal =
  LispDouble Double | LispRat LispRational | LispInteger Integer
  deriving (Eq, Show)

data LispRational = LispRational Integer Integer
  deriving (Eq, Show)

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


binToDec :: String -> Int
binToDec = foldl' (\acc x -> acc * 2 + digitToInt x) 0

parseInt :: Parser LispValue
parseInt = do
  x <- many1 digit
  return $ RealNumber (LispInteger (read x))

parseOctal :: Parser LispValue
parseOctal = do
  x <- many1 octDigit
  let first:_ = readOct x
  return $ RealNumber (LispInteger (fst first))

parseHex :: Parser LispValue
parseHex = do
  x <- many1 hexDigit
  let first:_ = readHex x
  return $ RealNumber (LispInteger (fst first))

parseDecimal :: Parser LispValue
parseDecimal = do
  x <- many1 digit
  char '.'
  y <- many digit
  let first:_ = readFloat (x ++ "." ++ y)
  return $ RealNumber (LispDouble (fst first))

parseBinary :: Parser LispValue
parseBinary = do
  x <- many1 (oneOf "01")
  return $ RealNumber (LispInteger (fromIntegral $ binToDec x))

parseNumber :: Parser LispValue
parseNumber =  try parseComplex
           <|> try parseReal
           <|> (char '#' >> parseNumberWithRadix)

parseNumberWithRadix :: Parser LispValue
parseNumberWithRadix = do
  t <- char 'o' <|> char 'x' <|> char 'd' <|> char 'b'
  case t of
    'o' -> parseOctal
    'b' -> parseBinary
    'x' -> parseHex
    'd' -> parseDecimal

parseExpr :: Parser LispValue
parseExpr =  try parseNumber
         <|> try (char '#' >> parseCharacter)
         <|> parseString
         <|> parseAtom

parseCharacter :: Parser LispValue
parseCharacter = do
  char '\\'
  first <- anyChar
  rest <- many (noneOf " ")
  return $ Character (first:rest)

parseRational :: Parser LispValue
parseRational = do
  RealNumber (LispInteger numerator) <- parseInt
  char '/'
  RealNumber (LispInteger denominator) <- parseInt
  return $ RationalNumber (LispRational numerator denominator)

parseReal :: Parser LispValue
parseReal = try parseRational <|> try parseDecimal <|> parseInt

parseComplex :: Parser LispValue
parseComplex = do
  RealNumber x <- parseReal
  char '+'
  RealNumber y <- parseReal
  char 'i'
  return $ ComplexNumber (LispComplex x y)
