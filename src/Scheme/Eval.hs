module Scheme.Eval (eval, trapError, extractValue) where

import Control.Monad.Except
import Data.Char

import Scheme.Types


eval :: LispValue -> ThrowsError LispValue
eval val@(String _)        = return val
eval val@(ComplexNumber _) = return val
eval val@(RealNumber _)    = return val
eval val@(Bool _)          = return val
eval (List [Atom "quote", val])          = return val
eval (List [Atom "boolean?", val])       = return $ isBoolean val
eval (List [Atom "string?", val])        = return $ isString val
eval (List [Atom "string->symbol", val]) = return $ stringToSymbol val
eval (List [Atom "number?", val])        = return $ isNum val
eval (List [Atom "char?", val])          = return $ isChar val
eval (List [Atom "symbol?", val])        = return $ isSymb val
eval (List [Atom "symbol->string", val]) = return $ symbolToString val
eval (List [Atom "vector?", val])        = return $ isVector val
eval (List [Atom "list?", val])          = return $ isList val
eval (List (Atom func : args))           = mapM eval args >>= apply func
eval badForm = throwError $ BadSpecialForm "Unrecognized special form" badForm

stringToSymbol :: LispValue -> LispValue
stringToSymbol (String x) = Atom x
stringToSymbol x          =
  case eval x of
    Right v -> stringToSymbol v

symbolToString :: LispValue -> LispValue
symbolToString (Atom x)                      = String x
symbolToString (List [Atom "quote", Atom x]) = String $ map toLower x
symbolToString x                             =
  case eval x of
    Right v -> symbolToString v

isList :: LispValue -> LispValue
isList (List _) = Bool True
isList _        = Bool False

isVector :: LispValue -> LispValue
isVector (Vector _) = Bool True
isVector _          = Bool False

isSymb :: LispValue -> LispValue
isSymb (Atom _)           = Bool True
isSymb (List (Atom _:xs)) = Bool $ all (\x -> toBool (isSymb x)) xs
isSymb _                  = Bool False

toBool :: LispValue -> Bool
toBool (Bool True)  = True
toBool (Bool False) = False
toBool _          = False

isChar :: LispValue -> LispValue
isChar (Character _) = Bool True
isChar _             = Bool False

isBoolean :: LispValue -> LispValue
isBoolean (Bool _) = Bool True
isBoolean _        = Bool False

isString :: LispValue -> LispValue
isString (String _) = Bool True
isString _          = Bool False

isNum :: LispValue -> LispValue
isNum (ComplexNumber _) = Bool True
isNum (RealNumber _)    = Bool True
isNum _                 = Bool False

apply :: String -> [LispValue] -> ThrowsError LispValue
apply func args = maybe (throwError $
                           NotFunction "Unrecognized primitive function args" func)
                        ($ args)
                        (lookup func primitivies)

primitivies :: [(String, [LispValue] -> ThrowsError LispValue)]
primitivies = [ ("+", numericBinop (+))
              , ("-", numericBinop (-))
              , ("*", numericBinop (*))
              , ("/", numericBinop div)
              , ("mod", numericBinop mod)
              , ("quotient", numericBinop quot)
              , ("remainder", numericBinop rem)
              -- , ("=", numBoolBinop (==))
              -- , ("<", numBoolBinop (<))
              -- , (">", numBoolBinop (>))
              -- , ("/=", numBoolBinop (/=))
              -- , (">=", numBoolBinop (>=))
              -- , ("<=", numBoolBinop (<=))
              -- , ("&&", boolBoolBinop (&&))
              -- , ("||", boolBoolBinop (||))
              -- , ("string=?", strBoolBinop (==))
              -- , ("string<?", strBoolBinop (<))
              -- , ("string>?", strBoolBinop (>))
              -- , ("string<=?", strBoolBinop (<=))
              -- , ("string>=?", strBoolBinop (>=))
              ]

numericBinop :: (Integer -> Integer -> Integer) -> [LispValue] -> ThrowsError LispValue
numericBinop _ []              = throwError $ NumArgs 2 []
numericBinop _ singleValue@[_] = throwError $ NumArgs 2 singleValue
numericBinop op params         = mapM unpackNum params >>= return . RealNumber . LispInteger . foldl1 op

unpackNum :: LispValue -> ThrowsError Integer
unpackNum (RealNumber (LispInteger n)) = return n
unpackNum (String n)                   = let parsed = reads n in
                                             if null parsed
                                               then throwError $ TypeMismatch "number" $ String n
                                               else return $ fst $ parsed !! 0
unpackNum (List [n])                   = unpackNum n
unpackNum notNum                       = throwError $ TypeMismatch "number" notNum

trapError :: (MonadError a m, Show a) => m String -> m String
trapError action = catchError action (return . show)

-- We purposely leave extractValue undefined for a Left constructor,
-- because that represents a programmer error. We intend to use
-- extractValue only after a catchError, so it's better to fail fast
-- than to inject bad values into the rest of the program.
extractValue :: ThrowsError a -> a
extractValue (Right val) = val
