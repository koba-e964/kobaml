{-# LANGUAGE BangPatterns, DeriveDataTypeable #-}
module CDef where

import Data.Typeable
import Control.Exception
import Data.Map (Map)

{------------------
    Data Types for evaluation
-------------------}

newtype Name = Name String deriving (Eq, Show)

data Value = VInt Int
           | VBool Bool
           | VFun Name Env Expr
	   | VRFun Int [(Name, Name, Expr)] Env
           | VCons Value Value
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

type Env = Map String Value

{-------------------
    Exceptions
-------------------}

data ParseError = ParseError String deriving (Typeable, Show)

instance Exception ParseError
