module Eval where

data Value = VInt Int
           | VBool Bool deriving (Eq, Show)

data Expr  = EConst Value
     	   | EAdd Expr Expr
     	   | ESub Expr Expr
	   | EMul Expr Expr
	   | EDiv Expr Expr
	   | ELt  Expr Expr
	   | EIf  Expr Expr Expr deriving (Eq, Show)

op2Int :: (Int -> Int -> Int) -> Value -> Value -> Value
op2Int f (VInt v1) (VInt v2) = VInt (f v1 v2)
op2Int _ v1 v2 = evalError $ "int required, but got " ++ show v1 ++ ", " ++ show v2

type Env = ()

evalError :: String -> a
evalError str = error $ "Evaluation error:" ++ str

eval :: Env -> Expr -> Value
eval env (EConst v) = v
eval env (EAdd v1 v2) = op2Int (+) (eval env v1) (eval env v2)
eval env (ESub v1 v2) = op2Int (-) (eval env v1) (eval env v2)
eval env (EMul v1 v2) = op2Int (*) (eval env v1) (eval env v2)
eval env (EDiv v1 v2) = op2Int div (eval env v1) (eval env v2)
eval env (ELt e1 e2)  =
  let v1 = eval env e1
      v2 = eval env e2 in
  case (v1, v2) of
    (VInt i1, VInt i2) -> VBool $ i1 < i2
    _ 	      	       -> evalError $ "ELt got " ++ show v1 ++ ", " ++ show v2
eval env (EIf vc v1 v2) =
  case eval env vc of
    VBool b -> if b then (eval env v1) else (eval env v2)
    _	    -> evalError "EIf"
