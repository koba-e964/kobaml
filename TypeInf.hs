module TypeInf where

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

instance Show TypeVar where
  show (TypeVar x) = x


instance Show Type where
  show (TConc x) = x
  show (TFun x y) = "(" ++ show x ++ " -> " ++ show y ++ ")"
  show (TVar v) = "var(" ++ show v ++ ")"

type TypeMap = Map TypeVar Type

freeVars :: Type -> Set TypeVar {- free variables in type -}
freeVars (TConc _) = Set.empty
freeVars (TFun ty1 ty2) = Set.union (freeVars ty1) (freeVars ty2)
freeVars (TVar var) = Set.singleton var

unify :: Type -> Type -> TypeMap -> TypeMap

unify x y map
  | x == y = map
unify (TVar x) y map
  | Set.member x (freeVars y) = unifyError (TVar x) y 
  | otherwise                 = Map.insert x y map

unify y (TVar x) map
  | Set.member x (freeVars y) = unifyError y (TVar x)
  | otherwise                 = Map.insert x y map

unify (TFun a1 b1) (TFun a2 b2) map =
  let ss = unify a1 a2 map in
    unify (subst ss b1) (subst ss b2) ss
unify x y _ = unifyError x y

unifyError :: Type -> Type -> a
unifyError x y = error ("Cannot unify " ++ show x ++" with " ++ show y)


subst :: TypeMap -> Type -> Type
subst tmap (TVar v) =
  case Map.lookup v tmap of
    Just t -> t
    Nothing -> TVar v
subst tmap (TFun x y) = TFun (subst tmap x) (subst tmap y)
subst _    (TConc x)  = TConc x