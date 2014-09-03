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
import Data.Primitive.MutVar
import TypeInf

convertTypeError :: (Functor m, Monad m) => ExceptT TypeError m a -> ExceptT SomeError m a
convertTypeError = mapExceptT $ fmap $ either (Left . SEType) Right

convertEvalError :: (Functor m, Monad m) => ExceptT EvalError m a -> ExceptT SomeError m a
convertEvalError = mapExceptT $ fmap $ either (Left . SEEval) Right

runSt :: (Functor m, Monad m) => St m a -> ExceptT SomeError m a
runSt action = convertTypeError $ (mapExceptT $ \x -> fmap fst $ runStateT x 0) action

runEV :: EnvLazy IO -> EV IO a -> ExceptT SomeError IO (a, EnvLazy IO)
runEV env action = convertEvalError $ mapExceptT k action where
    k state' = do
      (res, newenv) <- runStateT state' env
      case res of
        Left x -> return $ Left x
        Right y -> return $ Right (y, newenv)


processExpr :: String -> TypeEnv -> EnvLazy IO -> Expr -> Bool -> ExceptT SomeError IO (TypeScheme, ValueLazy IO)
processExpr !name !tenv !venv !expr !showing = do
  ty <- runSt $ typeInfer tenv expr
  lift $ putStrLn $ name ++ " : " ++ show ty
  (result, _) <- runEV venv $  eval expr 
  (rs, _    ) <- runEV venv $ showValueLazy result
  when showing $ lift $ putStrLn $ " = " ++ rs
  ty `seq` result `seq` return (ty, result)

readCmd :: IO (Either ParseError Command)
readCmd = do
  line <- getLine
  return $ commandOfString line

repl :: TypeEnv -> EnvLazy IO -> IO ()
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

processCmdShow :: Command -> TypeEnv -> EnvLazy IO -> Bool -> ExceptT SomeError IO (TypeEnv, EnvLazy IO)
processCmdShow !cmd !tenv !venv !showing=
            case cmd of
                 CLet (Name name) expr -> do
                     (!ty, _) <- processExpr name tenv venv expr showing
                     let newtenv = Map.insert name ty     tenv
		     thunk <- lift $ newMutVar $ Thunk venv expr
                     let newvenv = LMap.insert name thunk venv
                     return (newtenv, newvenv)
                 CRLets bindings       -> do
                     newtenv <- runSt $ tyRLetBindingsInfer tenv bindings
                     (newvenv, _) <- runEV venv $  getNewEnvInRLets bindings venv
                     lift $ forM_ bindings $ \(Name fname,  _) -> do
                       let Just ty = Map.lookup fname newtenv
                       putStrLn $ fname ++ " : " ++ show ty
                     return (newtenv, newvenv)
                 CExp expr -> processExpr "-" tenv venv expr showing >> return (tenv, venv)
                 CQuit     -> if showing then error "(>_<)(>_<)" else return (tenv, venv)


processCmd :: Command -> TypeEnv -> EnvLazy IO -> ExceptT SomeError IO (TypeEnv, EnvLazy IO)
processCmd !cmd !tenv !venv = processCmdShow cmd tenv venv True

nextEnv :: Command -> (TypeEnv, EnvLazy IO) -> ExceptT SomeError IO (TypeEnv, EnvLazy IO)
nextEnv !cmd (!tenv, !venv) = processCmdShow cmd tenv venv False

loadFile :: FilePath -> (TypeEnv, EnvLazy IO) -> IO (TypeEnv, EnvLazy IO)
loadFile path (tenv, env) = do
    cont <- readFile path
    case commandsOfString cont of
      Left x -> error $ "Error in loading \"" ++ path ++ "\":\n" ++ show x
      Right cmds -> sub cmds (tenv, env)
        where
	  sub [] (te, ve) = return (te, ve) :: IO (TypeEnv, EnvLazy IO)
	  sub (y : ys) (te, ve) = do
              Right newtve   <- runExceptT $ catchError (nextEnv y (te, ve)) (\_ -> return (te, ve))
              sub ys newtve


main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  (tenv, venv) <- loadFile "stdlib.txt" (teEmpty, LMap.empty)
  repl tenv venv
  return ()
