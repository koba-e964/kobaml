{-# LANGUAGE BangPatterns #-}
module Main where

import Control.Monad.State
import System.IO

import CDef
import Eval
import ExprParser
import qualified Data.Map as Map
import TypeInf

processExpr :: String -> TypeEnv -> Env -> Expr -> St IO (TypeScheme, Value)
processExpr !name !tenv !venv !expr = do
  ty <- typeInfer tenv expr
  lift $ putStrLn $ name ++ " : " ++ show ty
  let result = eval venv expr
  lift $ putStrLn $ " = " ++ show result
  ty `seq` result `seq` return (ty, result)

readCmd :: IO (Either ParseError Command)
readCmd = do
  line <- getLine
  return $ commandOfString line

repl :: TypeEnv -> Env -> St IO ()
repl !tenv !venv = do
    lift $ putStr "> "
    cmdOrErr <- lift $ readCmd
    case cmdOrErr of
        Left (ParseError ex) -> lift (putStrLn ex) >> repl tenv venv
        Right cmd -> 
            case cmd of
                 CLet (Name name) expr -> do
                     (!ty, !result) <- processExpr name tenv venv expr
                     let newtenv = Map.insert name ty     tenv
                     let newvenv = Map.insert name result venv
                     repl newtenv newvenv
                 CRLets bindings       -> do
                     newtenv <- tyRLetBindingsInfer tenv bindings
                     let newvenv = getNewEnvInRLets bindings venv
                     repl newtenv newvenv
                 CExp expr -> processExpr "-" tenv venv expr >> repl tenv venv
                 CQuit     -> return ()
main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  runStateT (repl teEmpty Map.empty) 0 >> return ()
