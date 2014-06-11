{-# LANGUAGE BangPatterns #-}
module Main where

import Control.Monad.State
import System.IO

import CDef
import Eval
import ExprParser
import qualified Data.Map.Strict as Map
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
                     lift $ forM_ bindings $ \(Name fname,  _) -> do
                       let Just ty = Map.lookup fname newtenv
                       putStrLn $ fname ++ " : " ++ show ty
                     repl newtenv newvenv
                 CExp expr -> processExpr "-" tenv venv expr >> repl tenv venv
                 CQuit     -> return ()

nextEnv :: Command -> (TypeEnv, Env) -> St IO (TypeEnv, Env)
nextEnv !cmd (!tenv, !venv) =
	case cmd of
            CLet (Name name) expr -> do
                     (!ty, !result) <- processExpr name tenv venv expr
                     let newtenv = Map.insert name ty     tenv
                     let newvenv = Map.insert name result venv
                     return (newtenv, newvenv)
            CRLets bindings       -> do
                     newtenv <- tyRLetBindingsInfer tenv bindings
                     let newvenv = getNewEnvInRLets bindings venv
                     lift $ forM_ bindings $ \(Name fname,  _) -> do
                       let Just ty = Map.lookup fname newtenv
                       putStrLn $ fname ++ " : " ++ show ty
                     return (newtenv, newvenv)
            CExp  _   -> return (tenv, venv)
            CQuit     -> return (tenv, venv)

loadFile :: FilePath -> (TypeEnv, Env) -> IO (TypeEnv, Env)
loadFile path (tenv, env) = do
    cont <- readFile path
    putStrLn cont
    case commandsOfString cont of
      Left x -> error $ "Error in loading \"" ++ path ++ "\":\n" ++ show x
      Right cmds -> fmap fst $ runStateT (sub cmds (tenv, env)) 0
        where
	  sub [] (te, ve) = return (te, ve) :: St IO (TypeEnv, Env)
	  sub (y : ys) (te, ve) = join $ fmap (sub ys) (nextEnv y (te, ve))


main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  (tenv, venv) <- loadFile "stdlib.txt" (teEmpty, Map.empty)
  runStateT (repl tenv venv) 0 >> return ()
