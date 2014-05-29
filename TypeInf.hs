module TypeInf where

import qualified Data.List as List 
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set

import Prelude hiding (map)

data Type = 
    TConc String -- concrete type
  | TFun Type Type
  | TVar TypeVar
  deriving Eq

intType :: Type
intType = TConc "int"
boolType :: Type
boolType = TConc "bool"

data TypeVar = TypeVar String deriving (Eq, Ord)
type TypeCons = (Type, Type)

instance Show TypeVar where
  show (TypeVar x) = x


instance Show Type where
  show (TConc x) = x
  show (TFun x y) = case x of
    (TFun _ _) -> "(" ++ show x ++ ") -> " ++ show y 
    _          -> show x ++ " -> " ++ show y
  show (TVar v) = "'" ++ show v

type TypeMap = Map TypeVar Type

freeVars :: Type -> Set TypeVar {- free variables in type -}
freeVars (TConc _) = Set.empty
freeVars (TFun ty1 ty2) = Set.union (freeVars ty1) (freeVars ty2)
freeVars (TVar var) = Set.singleton var

addCons :: TypeVar -> Type -> (TypeMap, [TypeCons]) -> (TypeMap, [TypeCons])
addCons var ty (tmap, cons) =
  let inc = subst (Map.singleton var ty)
      conv = Map.insert var ty $ Map.map inc tmap
      newcons = List.map (\(ty1, ty2) -> (inc ty1, inc ty2)) cons in
    (conv, newcons)

unifyAll :: [TypeCons] -> TypeMap -> TypeMap
unifyAll cons map = sub map cons where
         sub m [] = m
         sub m ((ty1, ty2):rest) = let (newm, newrest) = unify ty1 ty2 (m, rest) in sub newm newrest


unify :: Type -> Type -> (TypeMap, [TypeCons]) -> (TypeMap, [TypeCons])

unify x y map
  | x == y = map
unify (TVar x) y map
  | Set.member x (freeVars y) = errorRecursive x y map
  | otherwise                 = addCons x y map

unify y (TVar x) map
  | Set.member x (freeVars y) = errorRecursive x y map
  | otherwise                 = addCons x y map

unify (TFun a1 b1) (TFun a2 b2) (map, cons) =
  (map, (a1, a2) : (b1, b2) : cons)
unify x y _ = unifyError x y

unifyError :: Type -> Type -> a
unifyError x y = error ("Cannot unify " ++ show x ++" with " ++ show y ++ " (T_T)" )

errorRecursive :: TypeVar -> Type -> (TypeMap, [TypeCons]) -> a
errorRecursive tvar ty mapc = error $ "Cannot construct recursive type(>_<) :" ++ show tvar ++ " = " ++ show ty ++ " in " ++ show mapc


subst :: TypeMap -> Type -> Type
subst tmap (TVar v) =
  case Map.lookup v tmap of
    Just t -> t
    Nothing -> TVar v
subst tmap (TFun x y) = TFun (subst tmap x) (subst tmap y)
subst _    (TConc x)  = TConc x
