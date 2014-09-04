{- LANGUAGE BangPatterns -}
module EvalLazy where

import Control.Monad.Except
import Control.Monad.Trans.Maybe
import Control.Monad.State
import Control.Monad.Primitive
import qualified Data.Map as Map
import Data.Primitive.MutVar
import Data.List (foldl', intercalate)

import CDef

type EV m = ExceptT EvalError (StateT (EnvLazy m) m)
liftToEV :: (PrimMonad m, Monad m) => m a -> EV m a
liftToEV = lift . lift

op2Int :: (PrimMonad m, Monad m) => (Int -> Int -> Int) -> ValueLazy m -> ValueLazy m -> EV m (ValueLazy m)
op2Int f (VLInt v1) (VLInt v2) = return $ VLInt (f v1 v2)
op2Int _ _v1 _v2 = evalError "int required"

-- The same as op2Int, but checks the second argument and returns error if it is 0.
op2IntDiv :: (PrimMonad m, Monad m) => (Int -> Int -> Int) -> ValueLazy m -> ValueLazy m -> EV m (ValueLazy m)
op2IntDiv f (VLInt v1) (VLInt v2)
  | v2 == 0   = evalError $ "Division by zero: " ++ show v1 ++ " / 0"
  | otherwise = return $ VLInt (f v1 v2)
op2IntDiv _ _v1 _v2 = evalError "int required"

op2IntBool :: (PrimMonad m, Monad m) => (Int -> Int -> Bool) -> ValueLazy m -> ValueLazy m -> EV m (ValueLazy m)
op2IntBool f (VLInt v1) (VLInt v2) = return $ VLBool (f v1 v2)
op2IntBool _ _v1 _v2 = evalError "int required"


evalError :: (PrimMonad m, Monad m) => String -> EV m a
evalError str = throwError $ EvalError str

-- | Evaluates expr in the given environment.
-- | The environment will be restored when evaluation finishes.
evalInEnv :: (PrimMonad m, Monad m) => Expr -> EnvLazy m -> EV m (ValueLazy m)
evalInEnv expr newvenv = do
    oldvenv <- get
    put newvenv
    res <- eval expr
    put oldvenv
    return res

eval :: (PrimMonad m, Monad m) => Expr -> EV m (ValueLazy m)
eval (EConst (VInt v)) = return $ VLInt v
eval (EConst (VBool v)) = return $ VLBool v
eval (EConst _) = error "(>_<) weird const expression..."
eval (EVar (Name name)) = do
    env <- get
    case Map.lookup name env of
      Just thunk -> evalThunk thunk
      Nothing    -> evalError $ "Unbound variable: " ++ name
eval (EAdd v1 v2) = join $ op2Int (+) `liftM` eval v1 `ap` eval v2
eval (ESub v1 v2) = join $ op2Int (-) `liftM` eval v1 `ap` eval v2
eval (EMul v1 v2) = join $ op2Int (*) `liftM` eval v1 `ap` eval v2
eval (EDiv v1 v2) = join $ op2IntDiv div `liftM` eval v1 `ap` eval v2
eval (EMod v1 v2) = join $ op2IntDiv mod `liftM` eval v1 `ap` eval v2
eval (ELt e1 e2)  = join $ op2IntBool (<) `liftM` eval e1 `ap` eval e2
eval (EEq e1 e2)  = join $ op2IntBool (==) `liftM` eval e1 `ap` eval e2
eval (EIf vc v1 v2) = do
  cond <- eval vc
  case cond of
    VLBool b -> if b then eval v1 else eval v2
    _	    -> evalError "EIf"
eval (ELet (Name name) ei eo) = do
    env <- get
    thunk <- liftToEV $ newMutVar (Thunk env ei)
    let newenv = Map.insert name thunk env
    evalInEnv eo newenv
eval (ERLets bindings expr) = do
     env <- get
     newenv <- getNewEnvInRLets bindings env
     evalInEnv expr newenv
eval (EMatch expr patex) = do
     env <- get
     thunk <- liftToEV $ newMutVar $ Thunk env expr
     tryMatchAll thunk env patex
eval (EFun name expr) = do
     env <- get
     return $ VLFun name env expr
eval (EApp func argv) = do
     env <- get
     join $ evalApp `liftM` eval func `ap` liftToEV (newMutVar (Thunk env argv))
eval (ECons e1 e2) = constructFromThunks "::" [e1, e2]
eval (EPair e1 e2) = constructFromThunks ","  [e1, e2]
eval ENil          = constructFromThunks "[]" []
eval (ESeq ea eb)  = do
     _ <- eval ea
     eval eb
eval (EStr str) = return $ VLStr str

constructFromThunks :: (PrimMonad m, Monad m) => String -> [Expr] -> EV m (ValueLazy m)
constructFromThunks ctor es = do
     env <- get
     ts <- mapM (createThunk env) es
     return $ VLCtor ctor ts

evalApp :: (PrimMonad m, Monad m) => ValueLazy m -> Thunk m -> EV m (ValueLazy m)
evalApp fval ath =
  case fval of
    VLFun (Name param) fenv expr -> do
      oldenv <- get
      put $ Map.insert param ath fenv
      ret <- eval expr
      put oldenv
      return ret
    _others                      -> evalError "app: not a function" 

getNewEnvInRLets :: (PrimMonad m, Monad m) => [(Name, Expr)] -> EnvLazy m -> EV m (EnvLazy m)
getNewEnvInRLets bindings oldenv = mnewenv where
  mnewenv = sub oldenv bindings
  sub env [] = return env
  sub env ((Name fname, fexpr) : rest) = do
        thunk <- liftToEV $ newMutVar $ Thunk env (ERLets bindings fexpr)
        sub (Map.insert fname thunk env) rest

tryMatchAll :: (PrimMonad m, Monad m) => Thunk m -> EnvLazy m -> [(Pat, Expr)] -> EV m (ValueLazy m)
tryMatchAll _    _  []                   = evalError "Matching not exhaustive"
tryMatchAll thunk env ((pat, expr) : rest) = do
  pickOne <- runMaybeT $ tryMatch thunk env pat
  case pickOne of
    Nothing     -> tryMatchAll thunk env rest
    Just newenv -> do
       put newenv
       ret <- eval expr
       put env
       return ret

tryMatch :: (PrimMonad m, Monad m) => Thunk m -> EnvLazy m -> Pat -> MaybeT (EV m) (EnvLazy m)
tryMatch thunk env pat = case pat of
  PConst (VBool b) -> do
    val <- lift $ evalThunk thunk
    case val of
      VLBool c | c == b -> return $ env 
      _                 -> fail ""
  PConst (VInt b) -> do
    val <- lift $ evalThunk thunk
    case val of
      VLInt c | c == b -> return $ env
      _                -> fail ""
  PConst _     -> error "weird const pattern... (>_<)"
  PVar (Name vname) -> return  $! Map.insert vname thunk env
  PCtor pctor pargs   -> do
    val <- lift $ evalThunk thunk
    case val of
      VLCtor vctor vargs | vctor == pctor -> do
        -- assert that length vargs == length pargs
        exs <- forM [0 .. length pargs - 1] $ \i -> tryMatch (vargs !! i) env (pargs !! i)
        return $! foldl' Map.union env exs
      _notused       -> fail ""

evalThunk :: (PrimMonad m, Monad m) => Thunk m -> EV m (ValueLazy m)
evalThunk thunk = do
  dat <- liftToEV $ readMutVar thunk
  case dat of
    Thunk env expr -> do
      ret <- evalInEnv expr env
      liftToEV $ writeMutVar thunk (ThVal ret)
      return ret
    ThVal value -> return value

showValueLazy :: (PrimMonad m, Monad m) => ValueLazy m -> EV m String
showValueLazy (VLInt  v) = return $ show v
showValueLazy (VLBool v) = return $ show v
showValueLazy (VLFun (Name name) _ _) = return $ "fun " ++ name ++ " -> (expr)" 
showValueLazy (VLCtor "::" [tcar,tcdr]) = showListLazy tcar tcdr
showValueLazy (VLStr str) = return str
showValueLazy (VLCtor "," [a,b]) = showPairLazy a b
showValueLazy (VLCtor "[]" []) = return "[]"
showValueLazy (VLCtor ctor args) = do
    argsStr <- mapM (evalThunk >=> showValueLazy) args
    return $ "(" ++ ctor ++ " " ++ intercalate " " argsStr ++ ")"
showListLazy :: (PrimMonad m, Monad m) => Thunk m -> Thunk m -> EV m String
showListLazy tcar tcdr = do
    inner <- sub tcar tcdr (10 :: Int)
    return $ "[" ++ inner ++ "]" where
    sub _ _    0 = return "..."
    sub t1 tcdr' n = do
      v1 <- evalThunk t1
      vcdr <- evalThunk tcdr'
      case vcdr of
        VLCtor "[]" [] -> showValueLazy v1
        VLCtor "::" [t2, t3] -> do
          sv <- showValueLazy v1
          sr <- sub t2 t3 (n-1)
          return $ sv ++ ", " ++ sr
        _     -> error "(>_<) < weird... the last cell of the list is not nil..."

showPairLazy :: (PrimMonad m, Monad m) => Thunk m -> Thunk m -> EV m String
showPairLazy a b = do
    va <- evalThunk a
    sa <- showValueLazy va
    vb <- evalThunk b
    sb <- showValueLazy vb
    return $ "(" ++ sa ++ ", " ++ sb ++ ")"

createThunk :: (PrimMonad m, Monad m) => EnvLazy m -> Expr -> EV m (Thunk m)
createThunk venv expr = liftToEV $ newMutVar $ Thunk venv expr

