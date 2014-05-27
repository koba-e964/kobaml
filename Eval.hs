module Eval where

import qualified Data.Map as Map
import Data.Map (Map)

newtype Name = Name String deriving (Eq, Show)

data Value = VInt Int
           | VBool Bool
           | VFun Name Env Expr
           | VCons Value Value
           | VNil
           deriving (Eq, Show)

data Expr  = EConst Value
           | EVar Name
     	   | EAdd Expr Expr
     	   | ESub Expr Expr
	   | EMul Expr Expr
	   | EDiv Expr Expr
	   | ELt  Expr Expr
           | EEq  Expr Expr
	   | EIf Expr Expr Expr
           | ELet Name Expr Expr
           | ERLets [(Name, Name, Expr)] Expr 
           | EMatch Expr [(Pat, Expr)]
           | EFun Name Expr
           | EApp Expr Expr
           | ECons Expr Expr
           | ENil
           deriving (Eq, Show)
data Pat   = PConst Value
           | PVar   Name
           | PCons  Pat Pat
           | PNil
           deriving (Eq, Show)

data Command
  = CLet    Name Expr 
  | CRLets  [(Name, Name, Expr)]  
  | CExp    Expr 
  | CQuit
  deriving (Eq, Show)



op2Int :: (Int -> Int -> Int) -> Value -> Value -> Value
op2Int f (VInt v1) (VInt v2) = VInt (f v1 v2)
op2Int _ v1 v2 = evalError $ "int required, but got " ++ show v1 ++ ", " ++ show v2

op2IntBool :: (Int -> Int -> Bool) -> Value -> Value -> Value
op2IntBool f (VInt v1) (VInt v2) = VBool (f v1 v2)
op2IntBool _ v1 v2 = evalError $ "int required, but got " ++ show v1 ++ ", " ++ show v2

type Env = Map String Value

evalError :: String -> a
evalError str = error $ "Evaluation error:" ++ str

eval :: Env -> Expr -> Value
eval _   (EConst v) = v
eval env (EVar (Name name)) =
    case Map.lookup name env of
      Just value -> value
      Nothing    -> evalError $ "Unbound variable: " ++ name
eval env (EAdd v1 v2) = op2Int (+) (eval env v1) (eval env v2)
eval env (ESub v1 v2) = op2Int (-) (eval env v1) (eval env v2)
eval env (EMul v1 v2) = op2Int (*) (eval env v1) (eval env v2)
eval env (EDiv v1 v2) = op2Int div (eval env v1) (eval env v2)
eval env (ELt e1 e2)  = op2IntBool (<) (eval env e1) (eval env e2)
eval env (EEq e1 e2)  = op2IntBool (==) (eval env e1) (eval env e2)
eval env (EIf vc v1 v2) =
  case eval env vc of
    VBool b -> if b then (eval env v1) else (eval env v2)
    _	    -> evalError "EIf"
eval env (ELet (Name name) ei eo) =
    let newenv = Map.insert name (eval env ei) env in
      eval newenv eo
eval env (EFun name expr) = VFun name env expr
eval env (EApp func argv) =
    case eval env func of
      VFun (Name param) fenv expr -> eval (Map.insert param (eval env argv) fenv) expr
      _                           -> evalError $ "app: not a function: " ++ show func
eval env (ECons e1 e2) = VCons (eval env e1) (eval env e2)
eval env ENil          = VNil
