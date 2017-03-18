{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Control.Monad
import           Control.Monad.ST
import           Data.Char
import           Data.Foldable
import           Data.Functor
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as M
import           Numeric
import           System.Environment
import           Text.Parsec.Prim
import           Text.ParserCombinators.Parsec hiding (spaces, try)

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
               | Vector (V.Vector LispValue)
               | RealNumber LispReal
               | RationalNumber LispRational
               | ComplexNumber LispComplex
               | Character String
               | String String
               | Bool Bool
               deriving (Eq, Show)

data LispComplex =
  LispComplex LispReal Sign LispReal
  deriving (Eq, Show)

data LispReal =
  LispDouble Double | LispRat LispRational | LispInteger Integer
  deriving (Eq, Show)

data LispRational = LispRational Integer Integer
  deriving (Eq, Show)

data Sign = Positive | Negative
  deriving (Eq, Show)

parseString :: Parser LispValue
parseString = do
  char '"'
  let escape c = try $ string ("\\" ++ [c])
  x <- many1 $ choice [
      escape '"'  $> '"'
    , escape 'r'  $> '\r'
    , escape 'n'  $> '\n'
    , escape 't'  $> '\t'
    , escape '\\' $> '\\'
    , noneOf "\""
    ]
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
parseNumberWithRadix =
  choice [ char 'o' >> parseOctal
         , char 'b' >> parseBinary
         , char 'x' >> parseHex
         , char 'd' >> parseDecimal
         ]
  
parseExpr :: Parser LispValue
parseExpr =  try parseNumber
         <|> try (char '#' >> parseCharacter)
         <|> parseString
         <|> try parseVector
         <|> parseAtom
         <|> parseQuoted
         <|> parseQuasiQuote
         <|> do char '('
                x <- try parseList <|> parseDottedList
                char ')'
                return x

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
  sign <- oneOf "+-"
  RealNumber y <- parseReal
  char 'i'
  case sign of
    '+' -> return $ ComplexNumber (LispComplex x Positive y)
    '-' -> return $ ComplexNumber (LispComplex x Negative y)

parseList :: Parser LispValue
parseList = liftM List $ sepBy parseExpr spaces

parseDottedList :: Parser LispValue
parseDottedList = do
  head <- endBy parseExpr spaces
  tail <- char '.' >> spaces >> parseExpr
  return $ DottedList head tail

parseQuoted :: Parser LispValue
parseQuoted = do
  char '\''
  x <- parseExpr
  return $ List [Atom "quote", x]

parseQuasiQuote :: Parser LispValue
parseQuasiQuote = do
  char '`'
  x <- parseExpr
  return $ List [Atom "quasiquote", x]

parseVector :: Parser LispValue
parseVector = do
  char '#'
  char '('
  List x <- parseList
  char ')'
  return $ Vector (V.fromList x) 

updateVector :: V.Vector LispValue -> Int -> LispValue -> V.Vector LispValue
updateVector xs n x = runST $ do
  mvector <- V.unsafeThaw xs
  newmvector <- M.write mvector n x
  V.unsafeFreeze mvector
