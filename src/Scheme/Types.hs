module Scheme.Types where

import           Control.Monad.Except
import           Data.IORef
import qualified Data.Vector as V
import           Text.Parsec

data LispValue = Atom String
               | List [LispValue]
               | DottedList [LispValue] LispValue
               | Vector (V.Vector LispValue)
               | RealNumber LispReal
               | ComplexNumber LispComplex
               | Character Char
               | String String
               | Bool Bool
               deriving (Eq)

data LispComplex = LispComplex LispReal Sign LispReal
                 deriving (Eq, Show)

data LispReal = LispDouble Double
              | LispRational Integer Integer
              | LispInteger Integer
              deriving (Eq, Show)

data Sign = Positive
          | Negative
          deriving (Eq, Show)

instance Show LispValue where
  show = showVal

showVal :: LispValue -> String
showVal (String contents) = show contents
showVal (Atom name) = name
showVal (RealNumber (LispInteger number)) = show number
showVal (RealNumber (LispDouble number)) = show number
showVal (RealNumber (LispRational num denom)) = show num ++ "\\" ++ show denom
showVal (ComplexNumber (LispComplex x Positive y)) = show x ++ "+" ++ show y ++ "i"
showVal (Bool True) = "#t"
showVal (Bool False) = "#f"
showVal (List contents) = "(" ++ unwordsList contents ++ ")"
showVal (DottedList first rest) = "(" ++ unwordsList first ++ " . " ++ showVal rest ++ ")"

data LispError = NumArgs Integer [LispValue]
               | TypeMismatch String LispValue
               | Parser ParseError
               | BadSpecialForm String LispValue
               | NotFunction String String
               | UnboundVar String String
               | Default String

type ThrowsError = Either LispError

instance Show LispError where
  show = showError

showError :: LispError -> String
showError (UnboundVar message varname)  = message ++ ": " ++ varname
showError (BadSpecialForm message form) = message ++ ": " ++ show form
showError (NotFunction message func)    = message ++ ": " ++ show func
showError (NumArgs expected found)      = "Expected " ++ show expected
                                       ++ " args; found values " ++ unwordsList found
showError (TypeMismatch expected found) = "Invalid type: expected " ++ show expected
                                       ++ ", found  " ++ show found
showError (Parser parseErr)             = "Parser error at " ++ show parseErr
showError (Default msg)                 = "Default " ++ show msg

unwordsList :: [LispValue] -> String
unwordsList = unwords . map showVal

type Env = IORef [(String, IORef LispValue)]
type IOThrowsError = ExceptT LispError IO
