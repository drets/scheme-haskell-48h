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


data LispValue = Atom String
               | List [LispValue]
               | DottedList [LispValue] LispValue
               | Vector (V.Vector LispValue)
               | RealNumber LispReal
               | ComplexNumber LispComplex
               | Character String
               | String String
               | Bool Bool
               deriving (Eq)

instance Show LispValue where
  show = showVal

data LispComplex = LispComplex LispReal Sign LispReal
                 deriving (Eq, Show)

data LispReal = LispDouble Double
              | LispRational Integer Integer
              | LispInteger Integer
              deriving (Eq, Show)

data Sign = Positive
          | Negative
          deriving (Eq, Show)


main :: IO ()
main = getArgs >>= print . eval . readExpr . head

symbol :: Parser Char
symbol = oneOf "!#$%&|*+-/:<=>?@^_~"

readExpr :: String -> LispValue
readExpr input = case parse parseExpr "scheme" input of
  Left err -> String $ "No match: " ++ show err
  Right val -> val

spaces :: Parser ()
spaces = skipMany1 space

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
  s <- option '+' (char '-')
  x <- many1 digit
  case s of
    '+' -> return $ RealNumber (LispInteger (read x))
    '-' -> return $ RealNumber (LispInteger (negate (read x)))

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
parseExpr =
  choice [ try parseNumber
         , try (char '#' >> parseCharacter)
         , parseString
         , try parseVector
         , parseAtom
         , parseQuoted
         , parseQuasiQuote
         , do char '('
              x <- try parseList <|> parseDottedList
              char ')'
              return x
         ]

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

showVal :: LispValue -> String
showVal (String contents) = "\"" ++ contents ++ "\""
showVal (Atom name) = name
showVal (RealNumber (LispInteger number)) = show number
showVal (RealNumber (LispDouble number)) = show number
showVal (RealNumber (LispRational num denom)) = show num ++ "\\" ++ show denom
showVal (ComplexNumber (LispComplex x Positive y)) = show x ++ "+" ++ show y ++ "i"
showVal (Bool True) = "#t"
showVal (Bool False) = "#f"
showVal (List contents) = "(" ++ unwordsList contents ++ ")"
showVal (DottedList head tail) = "(" ++ unwordsList head ++ " . " ++ showVal tail ++ ")"

unwordsList :: [LispValue] -> String
unwordsList = unwords . map showVal

eval :: LispValue -> LispValue
eval val@(String _) = val
eval val@(ComplexNumber _) = val
eval val@(RealNumber _) = val
eval val@(Bool _) = val
eval (List [Atom "quote", val]) = val
eval (List (Atom func : args)) = apply func $ map eval args

apply :: String -> [LispValue] -> LispValue
apply func args = maybe (Bool False) ($ args) $ lookup func primitivies

primitivies :: [(String, [LispValue] -> LispValue)]
primitivies = [ ("+", numericBinop (+))
              , ("-", numericBinop (-))
              , ("*", numericBinop (*))
              , ("/", numericBinop div)
              , ("mod", numericBinop mod)
              , ("quotient", numericBinop quot)
              , ("remainder", numericBinop rem)
              ]

numericBinop :: (Integer -> Integer -> Integer) -> [LispValue] -> LispValue
numericBinop op params = RealNumber $ LispInteger $ foldl1 op $ map unpackNum params

unpackNum :: LispValue -> Integer
unpackNum (RealNumber (LispInteger n)) = n
unpackNum (String n) =
  let parsed = reads n :: [(Integer, String)] in
    if null parsed
       then 0
       else fst $ parsed !! 0
unpackNum (List [n]) = unpackNum n
unpackNum _ = 0


