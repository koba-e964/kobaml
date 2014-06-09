{-# LANGUAGE BangPatterns, DeriveDataTypeable #-}
module CDef where

import qualified Data.List as List 
import Data.Typeable
import Control.Exception
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
{------------------
    Data types for types
------------------}
data Type = 
    TConc !String -- concrete type
  | TFun !Type !Type
  | TVar !TypeVar
  | TList !Type
  | TPair !Type !Type
  deriving Eq

intType :: Type
intType = TConc "int"
boolType :: Type
boolType = TConc "bool"

data TypeVar = TypeVar !String deriving (Eq, Ord)
data TypeCons = TypeEqual !Type !Type deriving (Eq)

data TypeScheme = Forall !(Set TypeVar) !Type

type TypeSubst = Map TypeVar Type
type TypeEnv = Map String  TypeScheme

instance Show TypeVar where
  show (TypeVar x) = x
instance (Show TypeCons) where
  show (TypeEqual a b) = show a ++ " = " ++ show b

typeEqual :: Type -> Type -> TypeCons
typeEqual = TypeEqual

instance Show Type where
  show (TConc x) = x
  show (TFun x y) = case x of
    (TFun _ _) -> "(" ++ show x ++ ") -> " ++ show y 
    _          -> show x ++ " -> " ++ show y
  show (TVar v) = "'" ++ show v
  show (TList a) = "[" ++ show a ++ "]"
  show (TPair a b) = "(" ++ show a ++ ", " ++ show b ++ ")"
instance Show TypeScheme where
  show (Forall bvs ty)
    | Set.null bvs = show ty
    | otherwise    =
      "forall" ++ concat (List.map (\x -> " '" ++ show x) (Set.toList bvs)) ++
        ". " ++ show ty

tmEmpty :: TypeSubst
tmEmpty = Map.empty

teEmpty :: TypeEnv
teEmpty = Map.empty

-- | Generalizes @ty@ with no quantification.
fromType :: Type -> TypeScheme
fromType ty = Forall Set.empty ty

freeVars :: Type -> Set TypeVar {- free variables in type -}
freeVars (TConc _) = Set.empty
freeVars (TFun ty1 ty2) = Set.union (freeVars ty1) (freeVars ty2)
freeVars (TVar var) = Set.singleton var
freeVars (TList a) = freeVars a
freeVars (TPair ty1 ty2) = Set.union (freeVars ty1) (freeVars ty2)

freeVarsTypeScheme :: TypeScheme -> Set TypeVar
freeVarsTypeScheme (Forall vars ty) = Set.difference (freeVars ty) vars

{------------------
    Data Types for evaluation
-------------------}

newtype Name = Name String deriving (Eq, Show)

data Value = VInt Int
           | VBool Bool
           | VFun Name Env Expr
	   | VRFun Int [(Name, Name, Expr)] Env
           | VCons Value Value
           | VPair Value Value
           | VNil
           deriving (Eq)

instance (Show Value) where
  show (VInt  v) = show v
  show (VBool v) = show v
  show (VFun (Name name) _ _) = "fun " ++ name ++ " -> (expr)" 
  show (VRFun ind _ _) = "vrfun ind=" ++ show ind ++ " -> (expr)"
  show (VCons vcar vcdr) = "[" ++ sub vcar vcdr ++ "]" where
    sub !v VNil = show v
    sub !v1 (VCons v2 v3) = show v1 ++ ", " ++ sub v2 v3
    sub _  _ = error "(>_<) < weird... the last cell of the list is not nil..."
  show (VPair a b) = "(" ++ show a ++ ", " ++ show b ++ ")"
  show VNil = "[]"
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
           | EPair Expr Expr
           | ENil
           deriving (Eq, Show)
data Pat   = PConst Value
           | PVar   Name
           | PCons  Pat Pat
           | PPair  Pat Pat
           | PNil
           deriving (Eq, Show)

data Command
  = CLet    Name Expr 
  | CRLets  [(Name, Name, Expr)]  
  | CExp    Expr 
  | CQuit
  deriving (Eq, Show)

type Env = Map String Value

{-------------------
    Exceptions
-------------------}

data ParseError = ParseError String deriving (Typeable, Show)
instance Exception ParseError

data TypeError = TypeError String deriving (Typeable, Show)
instance Exception TypeError

data EvalError = EvalError String deriving (Typeable, Show)
instance Exception EvalError
