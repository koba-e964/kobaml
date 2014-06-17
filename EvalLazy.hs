{-# LANGUAGE BangPatterns #-}
module EvalLazy where

import Control.Applicative
import Control.Monad.Except
import qualified Data.Map as Map

import CDef hiding (Value, Env)

type Value = ValueLazy
type Env = EnvLazy

type EV m = ExceptT EvalError m

op2Int :: Monad m => (Int -> Int -> Int) -> Value -> Value -> EV m Value
op2Int f (VLInt v1) (VLInt v2) = return $ VLInt (f v1 v2)
op2Int _ v1 v2 = evalError $ "int required, but got " ++ show v1 ++ ", " ++ show v2

op2IntBool :: Monad m => (Int -> Int -> Bool) -> Value -> Value -> EV m Value
op2IntBool f (VLInt v1) (VLInt v2) = return $ VLBool (f v1 v2)
op2IntBool _ v1 v2 = evalError $ "int required, but got " ++ show v1 ++ ", " ++ show v2


evalError :: Monad m => String -> EV m a
evalError str = throwError $ EvalError $ str

eval :: (Functor m, Monad m) => Env -> Expr -> EV m Value
eval _   (EConst (VInt v)) = return $ VLInt v
eval _   (EConst (VBool v)) = return $ VLBool v
eval _   (EConst _) = error "(>_<) weird const expression..."
eval !env (EVar (Name name)) =
    case Map.lookup name env of
      Just value -> return value
      Nothing    -> evalError $ "Unbound variable: " ++ name
eval env (EAdd v1 v2) = join $ op2Int (+) <$> (eval env v1) <*> (eval env v2)
eval env (ESub v1 v2) = join $ op2Int (-) <$> (eval env v1) <*> (eval env v2)
eval env (EMul v1 v2) = join $ op2Int (*) <$> (eval env v1) <*> (eval env v2)
eval env (EDiv v1 v2) = join $ op2Int div <$> (eval env v1) <*> (eval env v2)
eval env (ELt e1 e2)  = join $ op2IntBool (<) <$> (eval env e1) <*> (eval env e2)
eval env (EEq e1 e2)  = join $ op2IntBool (==) <$> (eval env e1) <*> (eval env e2)
eval env (EIf vc v1 v2) = do
  cond <- eval env vc
  case cond of
    VLBool b -> if b then (eval env v1) else (eval env v2)
    _	    -> evalError "EIf"
eval env (ELet (Name name) ei eo) = do
    val <- eval env ei
    let newenv = Map.insert name val env
    eval newenv eo
eval env (ERLets bindings expr) = do
     newenv <- getNewEnvInRLets bindings env
     eval newenv expr
eval env (EMatch expr patex) = do
     val <- eval env expr
     tryMatchAll val env patex
eval env (EFun name expr) = return $ VLFun name env expr
eval env (EApp func argv) = join $ evalApp <$> (eval env func) <*> (eval env argv)
eval env (ECons e1 e2) = VLCons <$> eval env e1 <*> eval env e2
eval env (EPair e1 e2) = VLPair <$> eval env e1 <*> eval env e2
eval _   ENil          = return VLNil

evalApp :: (Functor m, Monad m) => Value -> Value -> EV m Value
evalApp fval aval =
  case fval of
    VLFun (Name param) fenv expr -> eval (Map.insert param aval fenv) expr
    others                       -> evalError $ "app: not a function: " ++ show others

getNewEnvInRLets :: (Functor m, Monad m) => [(Name, Expr)] -> Env -> EV m Env
getNewEnvInRLets bindings oldenv = mnewenv where
  mnewenv = sub oldenv bindings 0 
  sub env [] _ = return env
  sub env ((Name fname, fexpr) : rest) num = do
	newenv <- mnewenv
        thunk <- eval newenv fexpr
        sub (Map.insert fname thunk env) rest (num + 1)

tryMatchAll :: (Functor m, Monad m) => Value -> Env -> [(Pat, Expr)] -> EV m Value
tryMatchAll _    _  []                   = evalError "Matching not exhaustive"
tryMatchAll val env ((pat, expr) : rest) = case tryMatch val env pat of
  Nothing     -> tryMatchAll val env rest
  Just newenv -> eval newenv expr

tryMatch :: Value -> Env -> Pat -> Maybe Env
tryMatch val env pat = case pat of
  PConst (VBool b) -> case val of
      VLBool c -> if c == b then Just env else Nothing
      _	       -> Nothing
  PConst (VInt b) -> case val of
      VLInt c -> if c == b then Just env else Nothing
      _	      -> Nothing
  PVar (Name vname) -> Just $! Map.insert vname val env
  PCons pcar pcdr   -> case val of
    VLCons vcar vcdr -> do
      ex <- tryMatch vcar env pcar
      ey <- tryMatch vcdr env pcdr
      return $! Map.union ey (Map.union ex env)
    _ 	            -> Nothing
  PPair pfst psnd   -> case val of
    VLPair vfst vsnd -> do
      ex <- tryMatch vfst env pfst
      ey <- tryMatch vsnd env psnd
      return $! Map.union ey $ Map.union ex env
    _ 	            -> Nothing
  PNil              -> case val of
    VLNil  -> Just env
    _     -> Nothing
