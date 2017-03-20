{-# LANGUAGE ExistentialQuantification #-}
module Scheme.Eval (eval, trapError, extractValue) where

import Control.Monad.Except
import Data.Char

import Scheme.Types


eval :: LispValue -> ThrowsError LispValue
eval val@(String _)        = return val
eval val@(ComplexNumber _) = return val
eval val@(RealNumber _)    = return val
eval val@(Bool _)          = return val
eval (List [Atom "if", predic, conseq, alt]) = do
  result <- eval predic
  case result of
    Bool False -> eval alt
    Bool True  -> eval conseq
    x          -> throwError $ TypeMismatch "boolean" x
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
              , ("=", numBoolBinop (==))
              , ("<", numBoolBinop (<))
              , (">", numBoolBinop (>))
              , ("/=", numBoolBinop (/=))
              , (">=", numBoolBinop (>=))
              , ("<=", numBoolBinop (<=))
              , ("&&", boolBoolBinop (&&))
              , ("||", boolBoolBinop (||))
              , ("string=?", strBoolBinop (==))
              , ("string<?", strBoolBinop (<))
              , ("string>?", strBoolBinop (>))
              , ("string<=?", strBoolBinop (<=))
              , ("string>=?", strBoolBinop (>=))
              , ("car", car)
              , ("cdr", cdr)
              , ("cons", cons)
              , ("eq?", eqv)
              , ("eqv?", eqv)
              , ("equal?", equal)
              ]

boolBinop :: (LispValue -> ThrowsError a) -> (a -> a -> Bool) -> [LispValue] -> ThrowsError LispValue
boolBinop unpacker op args = if length args /= 2
                             then throwError $ NumArgs 2 args
                             else do left <- unpacker $ args !! 0
                                     right <- unpacker $ args !! 1
                                     return $ Bool $ left `op` right

numBoolBinop :: (Integer -> Integer -> Bool) -> [LispValue] -> ThrowsError LispValue
numBoolBinop = boolBinop unpackNum

strBoolBinop :: (String -> String -> Bool) -> [LispValue] -> ThrowsError LispValue
strBoolBinop = boolBinop unpackStr

boolBoolBinop :: (Bool -> Bool -> Bool) -> [LispValue] -> ThrowsError LispValue
boolBoolBinop = boolBinop unpackBool

unpackStr :: LispValue -> ThrowsError String
unpackStr (String s)                             = return s
unpackStr (RealNumber (LispInteger s))           = return $ show s
unpackStr (Bool s)                               = return $ show s
unpackStr (ComplexNumber (LispComplex x sign y)) = do
  x' <- unpackStr (RealNumber x)
  y' <- unpackStr (RealNumber y)
  case sign of
    Positive -> return $ x' ++ "+" ++ y'
    Negative -> return $ x' ++ "-" ++ y'
unpackStr notString                              = throwError $ TypeMismatch "string" notString

unpackBool :: LispValue -> ThrowsError Bool
unpackBool (Bool b) = return b
unpackBool notBool  = throwError $ TypeMismatch "boolean" notBool

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

car :: [LispValue] -> ThrowsError LispValue
car [List (x : _)]         = return x
car [DottedList (x : _) _] = return x
car [badArg]               = throwError $ TypeMismatch "pair" badArg
car badArgList             = throwError $ NumArgs 1 badArgList

cdr :: [LispValue] -> ThrowsError LispValue
cdr [List (_ : xs)]         = return $ List xs
cdr [DottedList [_] x]      = return x
cdr [DottedList (_ : xs) x] = return $ DottedList xs x
cdr [badArg]                = throwError $ TypeMismatch "pair" badArg
cdr badArgList              = throwError $ NumArgs 1 badArgList

cons :: [LispValue] -> ThrowsError LispValue
cons [x1, List []]            = return $ List [x1]
cons [x, List xs]             = return $ List $ x : xs
cons [x, DottedList xs xlast] = return $ DottedList (x : xs) xlast
cons [x1, x2]                 = return $ DottedList [x1] x2
cons badArgList               = throwError $ NumArgs 2 badArgList

eqv :: [LispValue] -> ThrowsError LispValue
eqv [(Bool arg1), (Bool arg2)]                   = return $ Bool $ arg1 == arg2
eqv [(RealNumber arg1), (RealNumber arg2)]       = return $ Bool $ arg1 == arg2
eqv [(ComplexNumber arg1), (ComplexNumber arg2)] = return $ Bool $ arg1 == arg2
eqv [(String arg1), (String arg2)]               = return $ Bool $ arg1 == arg2
eqv [(Atom arg1), (Atom arg2)]                   = return $ Bool $ arg1 == arg2
eqv [(DottedList xs x), (DottedList ys y)]       = eqv [List $ xs ++ [x], List $ ys ++ [y]]
eqv [(List arg1), (List arg2)]                   = compareList arg1 arg2 eqv
eqv [_, _]                                       = return $ Bool False
eqv badArgList                                   = throwError $ NumArgs 2 badArgList

data Unpacker = forall a. Eq a => AnyUnpacker (LispValue -> ThrowsError a)

compareList :: [LispValue] -> [LispValue] -> ([LispValue] -> ThrowsError LispValue) -> ThrowsError LispValue
compareList arg1 arg2 eq_func = return $ Bool $ (length arg1 == length arg2) &&
                                                (all eqvPair $ zip arg1 arg2)
  where
    eqvPair (x1, x2) =
      case eq_func [x1, x2] of
        Left _           -> False
        Right (Bool val) -> val

unpackEquals :: LispValue -> LispValue -> Unpacker -> ThrowsError Bool
unpackEquals arg1 arg2 (AnyUnpacker unpacker) = (do
  unpacked1 <- unpacker arg1
  unpacked2 <- unpacker arg2
  return $ unpacked1 == unpacked2) `catchError` (const $ return False)

equal :: [LispValue] -> ThrowsError LispValue
equal [(DottedList xs x), (DottedList ys y)] = equal [List $ xs ++ [x], List $ ys ++ [y]]
equal [(List arg1), (List arg2)]             = compareList arg1 arg2 equal
equal [arg1, arg2] = do
  primitiveEquals <- liftM or $ mapM (unpackEquals arg1 arg2)
                     [AnyUnpacker unpackNum, AnyUnpacker unpackStr, AnyUnpacker unpackBool]
  eqvEquals <- eqv [arg1, arg2]
  return $ Bool $ (primitiveEquals || let (Bool x) = eqvEquals in x)
equal badArgList = throwError $ NumArgs 2 badArgList
