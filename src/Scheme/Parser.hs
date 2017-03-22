{-# LANGUAGE FlexibleContexts, OverloadedStrings #-}
module Scheme.Parser (parseExpr, readExpr) where

import           Control.Monad
import           Control.Monad.Except
import           Data.Char
import           Data.Foldable
import           Data.Functor
import qualified Data.Vector as V
import           Numeric
import           Text.Parsec.Prim
import           Text.ParserCombinators.Parsec hiding (spaces, try)

import           Scheme.Types


parseExpr :: Parser LispValue
parseExpr =
  choice [ try parseNumber
         , try (char '#' >> parseCharacter)
         , try parseVector
         , parseString
         , parseAtom
         , parseQuoted
         , parseQuasiQuote
         , do char '('
              x <- try parseList <|> parseDottedList
              char ')'
              return x
         ]

readExpr :: String -> ThrowsError LispValue
readExpr input = case parse parseExpr "scheme" input of
  Left err -> throwError $ Parser err
  Right val -> return val

symbol :: Parser Char
symbol = oneOf "!#$%&|*+-/:<=>?@^_~"

spaces :: Parser ()
spaces = skipMany1 space

parseString :: Parser LispValue
parseString = do
  char '"'
  let escape c = try $ string ("\\" ++ [c])
  x <- many $ choice [
      escape '"'  $> '"'
    , escape 'r'  $> '\r'
    , escape 'n'  $> '\n'
    , escape 't'  $> '\t'
    , escape '\\' $> '\\'
    ] <|> noneOf "\""
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
  s <- option '+' (char '-')
  x <- many1 digit
  case s of
    '+' -> return $ RealNumber (LispInteger (read x))
    '-' -> return $ RealNumber (LispInteger (negate (read x)))
    _   -> error "shouldn't happen"

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
  s <- option '+' (char '-')
  x <- many1 digit
  char '.'
  y <- many1 digit
  let first:_ = readFloat (x ++ "." ++ y)
  case s of
    '+' -> return $ RealNumber (LispDouble (fst first))
    '-' -> return $ RealNumber (LispDouble (negate (fst first)))
    _   -> error "shouldn't happen"


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

parseCharacter :: Parser LispValue
parseCharacter = do
  char '\\'
  first <- anyChar
  rest <- many (noneOf " )")
  case rest of
    [] -> return $ Character first
    _  -> case map toLower (first:rest) of
      "space"   -> return $ Character ' '
      "newline" -> return $ Character '\n'
      "tab"     -> return $ Character '\t'
      x         -> error $ "invalid character name" ++ show x

parseRational :: Parser LispValue
parseRational = do
  RealNumber (LispInteger numerator) <- parseInt
  char '/'
  RealNumber (LispInteger denominator) <- parseInt
  return $ RealNumber (LispRational numerator denominator)

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
    _   -> error "shouldn't happen"


parseList :: Parser LispValue
parseList = liftM List $ sepBy parseExpr spaces

parseDottedList :: Parser LispValue
parseDottedList = do
  first <- endBy parseExpr spaces
  rest <- char '.' >> spaces >> parseExpr
  return $ DottedList first rest

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
  string "'#("
  List x <- parseList
  char ')'
  return $ Vector (V.fromList x)
