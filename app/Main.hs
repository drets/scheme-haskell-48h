module Main where

import Control.Monad
import Control.Monad.Except
import Data.IORef
import System.Environment
import System.IO

import Scheme.Eval
import Scheme.Parser
import Scheme.Types

main :: IO ()
main = do
  args <- getArgs
  if null args
    then runRepl
    else runOne $ args

flushStr :: String -> IO ()
flushStr str = putStr str >> hFlush stdout

readPromt :: String -> IO String
readPromt prompt = flushStr prompt >> getLine

evalString :: Env -> String -> IO String
evalString env expr = runIOThrows $ liftM show $ (liftThrows $ readExpr expr) >>= eval env

evalAndPrint :: Env -> String -> IO ()
evalAndPrint env expr = evalString env expr >>= putStrLn

until_ :: Monad m => (a -> Bool) -> m a -> (a -> m ()) -> m ()
until_ predic prompt action = do
  result <- prompt
  if predic result
    then return ()
    else action result >> until_ predic prompt action

runIOThrows :: IOThrowsError String -> IO String
runIOThrows action = runExceptT (trapError action) >>= return . extractValue

runOne :: [String] -> IO ()
runOne args = do
  env <- primitiveBindings >>= flip bindVars [("args", List $ map String $ drop 1 args)]
  (runIOThrows $ liftM show $ eval env (List [Atom "load", String (args !! 0)]))
    >>= hPutStrLn stderr

runRepl :: IO ()
runRepl = primitiveBindings >>= until_ (== "quit") (readPromt "Lisp>>> ") . evalAndPrint
