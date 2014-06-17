{-# LANGUAGE BangPatterns #-}
module Main where

import Control.Monad.State
import Control.Monad.Except
import System.IO

import CDef hiding (Value, Env)
import EvalLazy
import ExprParser
import qualified Data.Map as LMap
import qualified Data.Map.Strict as Map
import TypeInf

convertTypeError :: (Functor m, Monad m) => ExceptT TypeError m a -> ExceptT SomeError m a
convertTypeError = mapExceptT $ fmap $ either (Left . SEType) Right

convertEvalError :: (Functor m, Monad m) => ExceptT EvalError m a -> ExceptT SomeError m a
convertEvalError = mapExceptT $ fmap $ either (Left . SEEval) Right

runSt :: (Functor m, Monad m) => St m a -> ExceptT SomeError m a
runSt action = convertTypeError $ (mapExceptT $ \x -> fmap fst (runStateT x 0)) $ action


processExpr :: String -> TypeEnv -> Env -> Expr -> Bool -> ExceptT SomeError IO (TypeScheme, Value)
processExpr !name !tenv !venv !expr !showing = do
  ty <- runSt $ typeInfer tenv expr
  lift $ putStrLn $ name ++ " : " ++ show ty
  result <- convertEvalError $  eval venv expr
  when showing $ lift $ putStrLn $ " = " ++ show result
  ty `seq` result `seq` return (ty, result)

readCmd :: IO (Either ParseError Command)
readCmd = do
  line <- getLine
  return $ commandOfString line

liftIOToStIO :: IO a -> St IO a
liftIOToStIO x = lift $ lift x


repl :: TypeEnv -> Env -> IO ()
repl !tenv !venv = do
    putStr "> "
    cmdOrErr <- readCmd
    case cmdOrErr of
        Left (ParseError ex) -> putStrLn ex >> repl tenv venv
        Right cmd -> case cmd of
	    CQuit -> return ()
	    _	  -> do
	      tmp <- runExceptT $ processCmd cmd tenv venv
	      case tmp of
	          Left someError      -> do
		      putStrLn $ "error: " ++ show someError
		      repl tenv venv
	          Right (newtenv, newvenv) -> repl newtenv newvenv

processCmd :: Command -> TypeEnv -> Env -> ExceptT SomeError IO (TypeEnv, Env)
processCmd !cmd !tenv !venv =
            case cmd of
                 CLet (Name name) expr -> do
                     (!ty, !result) <- processExpr name tenv venv expr True
                     let newtenv = Map.insert name ty     tenv
                     let newvenv = LMap.insert name result venv
                     return (newtenv, newvenv)
                 CRLets bindings       -> do
                     newtenv <- runSt $ tyRLetBindingsInfer tenv bindings
                     newvenv <- convertEvalError $  getNewEnvInRLets bindings venv
                     lift $ forM_ bindings $ \(Name fname,  _) -> do
                       let Just ty = Map.lookup fname newtenv
                       putStrLn $ fname ++ " : " ++ show ty
                     return (newtenv, newvenv)
                 CExp expr -> processExpr "-" tenv venv expr True >> return (tenv, venv)
                 CQuit     -> error "(>_<)(>_<)"

nextEnv :: Command -> (TypeEnv, Env) -> ExceptT SomeError IO (TypeEnv, Env)
nextEnv !cmd (!tenv, !venv) =
	case cmd of
            CLet (Name name) expr -> do
                     (!ty, !result) <- processExpr name tenv venv expr False
                     let newtenv = Map.insert name ty     tenv
                     let newvenv = Map.insert name result venv
                     return (newtenv, newvenv)
            CRLets bindings       -> do
                     newtenv <- runSt $ tyRLetBindingsInfer tenv bindings
                     newvenv <- convertEvalError $ getNewEnvInRLets bindings venv
                     lift $ forM_ bindings $ \(Name fname,  _) -> do
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
              Right newtve   <- runExceptT $ catchError (nextEnv y (te, ve)) (\_ -> return (te, ve))
              sub ys newtve


main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  (tenv, venv) <- loadFile "stdlib.txt" (teEmpty, LMap.empty)
  repl tenv venv
  return ()
