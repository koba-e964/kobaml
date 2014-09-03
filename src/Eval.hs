{-# LANGUAGE BangPatterns #-}
module Eval where

import Control.Applicative
import Control.Monad.Except
import qualified Data.Map.Strict as Map

import CDef

type EV m = ExceptT EvalError m

op2Int :: Monad m => (Int -> Int -> Int) -> Value -> Value -> EV m Value
op2Int f (VInt v1) (VInt v2) = return $ VInt (f v1 v2)
op2Int _ v1 v2 = evalError $ "int required, but got " ++ show v1 ++ ", " ++ show v2

op2IntBool :: Monad m => (Int -> Int -> Bool) -> Value -> Value -> EV m Value
op2IntBool f (VInt v1) (VInt v2) = return $ VBool (f v1 v2)
op2IntBool _ v1 v2 = evalError $ "int required, but got " ++ show v1 ++ ", " ++ show v2


evalError :: Monad m => String -> EV m a
evalError str = throwError $ EvalError $ str

eval :: (Functor m, Monad m) => Env -> Expr -> EV m Value
eval _   (EConst v) = return v
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
    VBool b -> if b then (eval env v1) else (eval env v2)
    _	    -> evalError "EIf"
eval env (ELet (Name name) ei eo) = do
    val <- eval env ei
    let newenv = Map.insert name val env
    eval newenv eo
eval env (ERLets bindings expr) = eval (getNewEnvInRLets bindings env) expr
eval env (EMatch expr patex) = do
     val <- eval env expr
     tryMatchAll val env patex
eval env (EFun name expr) = return $ VFun name env expr
eval env (EApp func argv) = join $ evalApp <$> (eval env func) <*> (eval env argv)
eval env (ECons e1 e2) = VCons <$> eval env e1 <*> eval env e2
eval env (EPair e1 e2) = VPair <$> eval env e1 <*> eval env e2
eval _   ENil          = return VNil

evalApp :: (Functor m, Monad m) => Value -> Value -> EV m Value
evalApp fval aval =
  case fval of
    VFun (Name param) fenv expr -> eval (Map.insert param aval fenv) expr
    VRFun ind bindings fenv     -> do
      let (_, expr) = bindings !! ind
      let newenv = getNewEnvInRLets bindings fenv
      newfval <- eval newenv expr
      evalApp newfval aval
    others                      -> evalError $ "app: not a function: " ++ show others

getNewEnvInRLets :: [(Name, Expr)] -> Env -> Env
getNewEnvInRLets bindings oldenv = sub oldenv bindings 0 where
  sub env [] _ = env
  sub env ((Name fname, _) : rest) num =
    sub (Map.insert fname (VRFun num bindings env) env) rest (num + 1)

tryMatchAll :: (Functor m, Monad m) => Value -> Env -> [(Pat, Expr)] -> EV m Value
tryMatchAll _    _  []                   = evalError "Matching not exhaustive"
tryMatchAll val env ((pat, expr) : rest) = case tryMatch val env pat of
  Nothing     -> tryMatchAll val env rest
  Just newenv -> eval newenv expr

tryMatch :: Value -> Env -> Pat -> Maybe Env
tryMatch !val !env !pat = case pat of
  PConst v          -> if val == v then Just env else Nothing
  PVar (Name vname) -> Just $! Map.insert vname val env
  PCons pcar pcdr   -> case val of
    VCons vcar vcdr -> do
      ex <- tryMatch vcar env pcar
      ey <- tryMatch vcdr env pcdr
      return $! Map.union ey (Map.union ex env)
    _ 	            -> Nothing
  PPair pfst psnd   -> case val of
    VPair vfst vsnd -> do
      ex <- tryMatch vfst env pfst
      ey <- tryMatch vsnd env psnd
      return $! Map.union ey $ Map.union ex env
    _ 	            -> Nothing
  PNil              -> case val of
    VNil  -> Just env
    _     -> Nothing
