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
  case length args of
    0 -> runRepl
    1 -> runOne $ args !! 0
    _ -> putStrLn "Program takes only 0 or 1 arguments"

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

nullEnv :: IO Env
nullEnv = newIORef []

runOne :: String -> IO ()
runOne expr = primitiveBindings >>= flip evalAndPrint expr

runRepl :: IO ()
runRepl = primitiveBindings >>= until_ (== "quit") (readPromt "Lisp>>> ") . evalAndPrint

primitiveBindings :: IO Env
primitiveBindings = nullEnv >>= (flip bindVars $ map makePrimitiveFunc primitives)
  where
    makePrimitiveFunc (var, func) = (var, PrimitiveFunc func)
