{-# LANGUAGE BangPatterns #-}
module EvalLazy where

import Control.Applicative
import Control.Monad.Except
import Control.Monad.State
import Data.IORef
import qualified Data.Map as Map

import CDef hiding (Value, Env)

type Value = ValueLazy
type Env = EnvLazy

type EV = ExceptT EvalError (StateT EnvLazy IO)
liftToEV :: IO a -> EV a
liftToEV = lift . lift

op2Int :: (Int -> Int -> Int) -> Value -> Value -> EV Value
op2Int f (VLInt v1) (VLInt v2) = return $ VLInt (f v1 v2)
op2Int _ _v1 _v2 = evalError $ "int required"

op2IntBool :: (Int -> Int -> Bool) -> Value -> Value -> EV Value
op2IntBool f (VLInt v1) (VLInt v2) = return $ VLBool (f v1 v2)
op2IntBool _ _v1 _v2 = evalError $ "int required"


evalError :: String -> EV a
evalError str = throwError $ EvalError $ str

eval :: Expr -> EV Value
eval (EConst (VInt v)) = return $ VLInt v
eval (EConst (VBool v)) = return $ VLBool v
eval (EConst _) = error "(>_<) weird const expression..."
eval (EVar (Name name)) = do
    env <- get
    case Map.lookup name env of
      Just thunk -> do
        result <- evalThunk thunk
	return result
      Nothing    -> evalError $ "Unbound variable: " ++ name
eval (EAdd v1 v2) = join $ op2Int (+) <$> (eval v1) <*> (eval v2)
eval (ESub v1 v2) = join $ op2Int (-) <$> (eval v1) <*> (eval v2)
eval (EMul v1 v2) = join $ op2Int (*) <$> (eval v1) <*> (eval v2)
eval (EDiv v1 v2) = join $ op2Int div <$> (eval v1) <*> (eval v2)
eval (ELt e1 e2)  = join $ op2IntBool (<) <$> (eval e1) <*> (eval e2)
eval (EEq e1 e2)  = join $ op2IntBool (==) <$> (eval e1) <*> (eval e2)
eval (EIf vc v1 v2) = do
  cond <- eval vc
  case cond of
    VLBool b -> if b then (eval v1) else (eval v2)
    _	    -> evalError "EIf"
eval (ELet (Name name) ei eo) = do
    env <- get
    thunk <- liftToEV $ newIORef (Thunk env ei)
    let newenv = Map.insert name thunk env
    put newenv
    res <- eval eo
    put env
    return res
eval (ERLets bindings expr) = do
     env <- get
     newenv <- getNewEnvInRLets bindings env
     put newenv
     ret <- eval expr
     put env
     return ret
eval (EMatch expr patex) = do
     env <- get
     thunk <- liftToEV $ newIORef $ Thunk env expr
     tryMatchAll thunk env patex
eval (EFun name expr) = do
     env <- get
     return $ VLFun name env expr
eval (EApp func argv) = do
     env <- get
     join $ evalApp <$> (eval func) <*> liftToEV (newIORef (Thunk env argv))
eval (ECons e1 e2) = do
     env <- get
     t1 <- liftToEV $ newIORef $ Thunk env e1
     t2 <- liftToEV $ newIORef $ Thunk env e2
     return $ VLCons t1 t2
eval (EPair e1 e2) = do
     env <- get
     t1 <- liftToEV $ newIORef $ Thunk env e1
     t2 <- liftToEV $ newIORef $ Thunk env e2
     return $ VLPair t1 t2
eval ENil          = return VLNil
eval (ESeq ea eb)  = do
     _ <- eval ea
     eval eb

evalApp :: Value -> Thunk -> EV Value
evalApp fval ath =
  case fval of
    VLFun (Name param) fenv expr -> do
      oldenv <- get
      put $ Map.insert param ath fenv
      ret <- eval expr
      put oldenv
      return ret
    _others                      -> evalError $ "app: not a function" 

getNewEnvInRLets :: [(Name, Expr)] -> Env -> EV Env
getNewEnvInRLets bindings oldenv = mnewenv where
  mnewenv = sub oldenv bindings
  sub env [] = return env
  sub env ((Name fname, fexpr) : rest) = do
        thunk <- liftToEV $ newIORef $ Thunk env (ERLets bindings fexpr)
        sub (Map.insert fname thunk env) rest

tryMatchAll :: Thunk -> Env -> [(Pat, Expr)] -> EV Value
tryMatchAll _    _  []                   = evalError "Matching not exhaustive"
tryMatchAll thunk env ((pat, expr) : rest) = do
  pickOne <- tryMatch thunk env pat
  case pickOne of
    Nothing     -> tryMatchAll thunk env rest
    Just newenv -> do
       put newenv
       ret <- eval expr
       put env
       return ret

tryMatch :: Thunk -> Env -> Pat -> EV (Maybe Env)
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

evalThunk :: Thunk -> EV Value
evalThunk thunk = do
  dat <- liftToEV $ readIORef thunk
  case dat of
    Thunk env expr -> do
      oldenv <- get
      put env
      ret <- eval expr
      put oldenv
      liftToEV $ writeIORef thunk (ThVal ret)
      return ret
    ThVal value -> return value

showValueLazy :: Value -> EV String
showValueLazy (VLInt  v) = return $ show v
showValueLazy (VLBool v) = return $ show v
showValueLazy (VLFun (Name name) _ _) = return $ "fun " ++ name ++ " -> (expr)" 
showValueLazy (VLCons tcar tcdr) = do
    inner <- sub tcar tcdr (10 :: Int)
    return $ "[" ++ inner ++ "]" where
    sub _ _    0 = return "..."
    sub t1 tcdr' n = do
      v1 <- evalThunk t1
      vcdr <- evalThunk tcdr'
      case vcdr of
        VLNil -> showValueLazy v1
        VLCons t2 t3 -> do
          sv <- showValueLazy v1
          sr <- sub t2 t3 (n-1)
          return $ sv ++ ", " ++ sr
        _     -> error "(>_<) < weird... the last cell of the list is not nil..."
showValueLazy (VLPair a b) = do
    va <- evalThunk a
    sa <- showValueLazy va
    vb <- evalThunk b
    sb <- showValueLazy vb
    return $ "(" ++ sa ++ ", " ++ sb ++ ")"
showValueLazy VLNil = return "[]"

