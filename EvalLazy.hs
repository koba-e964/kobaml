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
op2Int _ _v1 _v2 = evalError $ "int required"

op2IntBool :: Monad m => (Int -> Int -> Bool) -> Value -> Value -> EV m Value
op2IntBool f (VLInt v1) (VLInt v2) = return $ VLBool (f v1 v2)
op2IntBool _ _v1 _v2 = evalError $ "int required"


evalError :: Monad m => String -> EV m a
evalError str = throwError $ EvalError $ str

eval :: (Functor m, Monad m) => Env -> Expr -> EV m Value
eval _   (EConst (VInt v)) = return $ VLInt v
eval _   (EConst (VBool v)) = return $ VLBool v
eval _   (EConst _) = error "(>_<) weird const expression..."
eval env (EVar (Name name)) =
    case Map.lookup name env of
      Just thunk -> evalThunk thunk
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
    let thunk = Thunk env ei
    let newenv = Map.insert name thunk env
    eval newenv eo
eval env (ERLets bindings expr) = do
     newenv <- getNewEnvInRLets bindings env
     eval newenv expr
eval env (EMatch expr patex) = do
     let thunk = Thunk env expr
     tryMatchAll thunk env patex
eval env (EFun name expr) = return $ VLFun name env expr
eval env (EApp func argv) = join $ evalApp <$> (eval env func) <*> return (Thunk env argv)
eval env (ECons e1 e2) = return $ VLCons (Thunk env e1) (Thunk env e2)
eval env (EPair e1 e2) = return $ VLPair (Thunk env e1) (Thunk env e2)
eval _   ENil          = return VLNil

evalApp :: (Functor m, Monad m) => Value -> Thunk -> EV m Value
evalApp fval ath =
  case fval of
    VLFun (Name param) fenv expr -> eval (Map.insert param ath fenv) expr
    _others                      -> evalError $ "app: not a function" 

getNewEnvInRLets :: (Functor m, Monad m) => [(Name, Expr)] -> Env -> EV m Env
getNewEnvInRLets bindings oldenv = mnewenv where
  mnewenv = sub oldenv bindings
  sub env [] = return env
  sub env ((Name fname, fexpr) : rest) = do
        let thunk = Thunk env (ERLets bindings fexpr)
        sub (Map.insert fname thunk env) rest

tryMatchAll :: (Functor m, Monad m) => Thunk -> Env -> [(Pat, Expr)] -> EV m Value
tryMatchAll _    _  []                   = evalError "Matching not exhaustive"
tryMatchAll thunk env ((pat, expr) : rest) = do
  pickOne <- tryMatch thunk env pat
  case pickOne of
    Nothing     -> tryMatchAll thunk env rest
    Just newenv -> eval newenv expr

tryMatch :: (Functor m, Monad m) => Thunk -> Env -> Pat -> EV m (Maybe Env)
tryMatch thunk env pat = case pat of
  PConst (VBool b) -> do
    val <- evalThunk thunk
    case val of
      VLBool c -> if c == b then return $ Just env else return Nothing
      _	       -> return Nothing
  PConst (VInt b) -> do
    val <- evalThunk thunk
    case val of
      VLInt c -> if c == b then return $ Just env else return Nothing
      _	      -> return Nothing
  PConst _     -> error "weird const pattern... (>_<)"
  PVar (Name vname) -> return $ Just $! Map.insert vname thunk env
  PCons pcar pcdr   -> do
    val <- evalThunk thunk
    case val of
      VLCons vcar vcdr -> do
        ex <- tryMatch vcar env pcar
        ey <- tryMatch vcdr env pcdr
        return $ do
          mex <- ex
          mey <- ey
          return $! Map.union mey (Map.union mex env)
      _notused       -> return Nothing
  PPair pfst psnd   -> do
    val <- evalThunk thunk
    case val of
      VLPair vfst vsnd -> do
        ex <- tryMatch vfst env pfst
        ey <- tryMatch vsnd env psnd
        return $ do
          mex <- ex
          mey <- ey
          return $! Map.union mey (Map.union mex env)
      _notused         -> return Nothing
  PNil              -> do
    val <- evalThunk thunk
    case val of
      VLNil  -> return $ Just env
      _      -> return Nothing

evalThunk :: (Functor m, Monad m) => Thunk -> EV m Value
evalThunk (Thunk env expr) = eval env expr


showValueLazy :: (Functor m, Monad m) => Value -> EV m String
showValueLazy (VLInt  v) = return $ show v
showValueLazy (VLBool v) = return $ show v
showValueLazy (VLFun (Name name) _ _) = return $ "fun " ++ name ++ " -> (expr)" 
showValueLazy (VLCons tcar tcdr) = do
    vcar <- evalThunk tcar
    vcdr <- evalThunk tcdr
    inner <- sub vcar vcdr (10 :: Int)
    return $ "[" ++ inner ++ "]" where
    sub _ _    0 = return "..."
    sub v VLNil _ = showValueLazy v
    sub v1 (VLCons t2 t3) n = do
      v2 <- evalThunk t2
      v3 <- evalThunk t3
      sv <- showValueLazy v1
      sr <- sub v2 v3 (n-1)
      return $ sv ++ ", " ++ sr
    sub _  _ _ = error "(>_<) < weird... the last cell of the list is not nil..."
showValueLazy (VLPair a b) = do
    va <- evalThunk a
    sa <- showValueLazy va
    vb <- evalThunk b
    sb <- showValueLazy vb
    return $ "(" ++ sa ++ ", " ++ sb ++ ")"
showValueLazy VLNil = return "[]"

