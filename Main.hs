{-# LANGUAGE BangPatterns #-}
module Main where

import Control.Monad.State
import Control.Monad.Error
import System.IO

import CDef
import Eval
import ExprParser
import qualified Data.Map.Strict as Map
import TypeInf

processExpr :: String -> TypeEnv -> Env -> Expr -> Bool -> St IO (TypeScheme, Value)
processExpr !name !tenv !venv !expr !showing = do
  ty <- typeInfer tenv expr
  liftIOToStIO $ putStrLn $ name ++ " : " ++ show ty
  let result = eval venv expr
  when showing $ lift $ lift $ putStrLn $ " = " ++ show result
  ty `seq` result `seq` return (ty, result)

readCmd :: IO (Either ParseError Command)
readCmd = do
  line <- getLine
  return $ commandOfString line

liftIOToStIO :: IO a -> St IO a
liftIOToStIO x = lift $ lift x


repl :: TypeEnv -> Env -> St IO ()
repl !tenv !venv = do
    liftIOToStIO $ putStr "> "
    cmdOrErr <- liftIOToStIO $ readCmd
    case cmdOrErr of
        Left (ParseError ex) -> liftIOToStIO (putStrLn ex) >> repl tenv venv
        Right cmd -> 
            case cmd of
                 CLet (Name name) expr -> do
                     (!ty, !result) <- processExpr name tenv venv expr True
                     let newtenv = Map.insert name ty     tenv
                     let newvenv = Map.insert name result venv
                     repl newtenv newvenv
                 CRLets bindings       -> do
                     newtenv <- tyRLetBindingsInfer tenv bindings
                     let newvenv = getNewEnvInRLets bindings venv
                     liftIOToStIO $ forM_ bindings $ \(Name fname,  _) -> do
                       let Just ty = Map.lookup fname newtenv
                       putStrLn $ fname ++ " : " ++ show ty
                     repl newtenv newvenv
                 CExp expr -> processExpr "-" tenv venv expr True >> repl tenv venv
                 CQuit     -> return ()

nextEnv :: Command -> (TypeEnv, Env) -> St IO (TypeEnv, Env)
nextEnv !cmd (!tenv, !venv) =
	case cmd of
            CLet (Name name) expr -> do
                     (!ty, !result) <- processExpr name tenv venv expr False
                     let newtenv = Map.insert name ty     tenv
                     let newvenv = Map.insert name result venv
                     return (newtenv, newvenv)
            CRLets bindings       -> do
                     newtenv <- tyRLetBindingsInfer tenv bindings
                     let newvenv = getNewEnvInRLets bindings venv
                     liftIOToStIO $ forM_ bindings $ \(Name fname,  _) -> do
                       let Just ty = Map.lookup fname newtenv
                       putStrLn $ fname ++ " : " ++ show ty
                     return (newtenv, newvenv)
            CExp  _   -> return (tenv, venv)
            CQuit     -> return (tenv, venv)

loadFile :: FilePath -> (TypeEnv, Env) -> IO (TypeEnv, Env)
loadFile path (tenv, env) = do
    cont <- readFile path
    case commandsOfString cont of
      Left x -> error $ "Error in loading \"" ++ path ++ "\":\n" ++ show x
      Right cmds -> sub cmds (tenv, env)
        where
	  sub [] (te, ve) = return (te, ve) :: IO (TypeEnv, Env)
	  sub (y : ys) (te, ve) = do
              (Right newtve,_)   <- runStateT (runErrorT $ catchError (nextEnv y (te, ve)) (\_ -> return (te, ve))) 0
              sub ys newtve


main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  (tenv, venv) <- loadFile "stdlib.txt" (teEmpty, Map.empty)
  runStateT (runErrorT $ catchError (repl tenv venv) (\(TypeError s) -> liftIOToStIO $ putStrLn s)) 0 >> return ()
