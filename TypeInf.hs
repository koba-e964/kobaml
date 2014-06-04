{-# LANGUAGE BangPatterns, TupleSections #-}
module TypeInf where

import qualified Data.List as List 
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Control.Monad.State
import Control.Monad.ST
import Data.STRef

import Eval

import Prelude hiding (map)

data Type = 
    TConc String -- concrete type
  | TFun Type Type
  | TVar TypeVar
  | TList Type
  deriving Eq

intType :: Type
intType = TConc "int"
boolType :: Type
boolType = TConc "bool"

data TypeVar = TypeVar String deriving (Eq, Ord)
data TypeCons = TypeEqual Type Type deriving (Eq)

data TypeScheme = Forall [TypeVar] Type

type TypeMap = Map TypeVar Type
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

instance Show TypeScheme where
  show (Forall bvs ty) = "forall" ++ concat (List.map (\x -> " '" ++ show x) bvs) ++ ". " ++ show ty

type St m = StateT Int m -- the state of the type inferrer

tmEmpty :: TypeMap
tmEmpty = Map.empty

teEmpty :: TypeEnv
teEmpty = Map.empty


freeVars :: Type -> Set TypeVar {- free variables in type -}
freeVars (TConc _) = Set.empty
freeVars (TFun ty1 ty2) = Set.union (freeVars ty1) (freeVars ty2)
freeVars (TVar var) = Set.singleton var
freeVars (TList a) = freeVars a

freeVarsTypeScheme :: TypeScheme -> Set TypeVar
freeVarsTypeScheme (Forall vars ty) = Set.difference (freeVars ty) (Set.fromList vars)

addCons :: TypeVar -> Type -> (TypeMap, [TypeCons]) -> (TypeMap, [TypeCons])
addCons var ty (tmap, cons) =
  let inc = subst (Map.singleton var ty)
      conv = Map.insert var ty $ Map.map inc tmap
      newcons = List.map (\(TypeEqual ty1 ty2) -> inc ty1 `typeEqual` inc ty2) cons in
    (conv, newcons)

unifyAll :: [TypeCons] -> TypeMap -> TypeMap
unifyAll cons map = sub map cons where
         sub m [] = m
         sub m ((TypeEqual ty1 ty2):rest) = let (newm, newrest) = unify ty1 ty2 (m, rest) in sub newm newrest


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
  (map, (a1 `typeEqual` a2) : (b1 `typeEqual` b2) : cons)
unify (TList a) (TList b) (map, cons) =
  (map, (a `typeEqual` b) : cons)
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
subst tmap (TList a) = TList (subst tmap a)

substTypeScheme :: TypeMap -> TypeScheme -> TypeScheme
substTypeScheme tmap (Forall vars ty) =
  let newtmap = List.foldl' (\m x -> Map.delete x m) tmap vars in
    Forall vars (subst newtmap ty)

newType :: (Monad m, Functor m) => St m Type
newType = do
  v <- newTypeVar
  return $ TVar v

newTypeVar :: (Monad m, Functor m) => St m TypeVar
newTypeVar = do
  v <- get
  put (v+1)
  return $ TypeVar $ "t" ++ show v

gatherConstraints :: (Monad m, Functor m) => TypeEnv -> Expr -> St m (TypeScheme, [TypeCons])

gatherConstraints !env !expr =
  case expr of
    EConst val -> case val of
      VInt _  -> return (Forall [] intType, [])
      VBool _ -> return (Forall [] boolType, [])
      _       -> undefined
    EVar (Name name) -> case Map.lookup name env of
      Just ty -> return $ (ty, [])
      Nothing -> error $ "unbound variable >_<"
    EAdd e1 e2 -> fmap (Forall [] intType,) $ gatherConsHelper env [(e1,intType), (e2, intType)]
    ESub e1 e2 -> fmap (Forall [] intType,) $ gatherConsHelper env [(e1,intType), (e2, intType)]
    EMul e1 e2 -> fmap (Forall [] intType,) $ gatherConsHelper env [(e1,intType), (e2, intType)]
    EDiv e1 e2 -> fmap (Forall [] intType,) $ gatherConsHelper env [(e1,intType), (e2, intType)]
    ELt  e1 e2 -> fmap (Forall [] boolType,) $ gatherConsHelper env [(e1,intType), (e2, intType)]
    EEq  e1 e2 -> fmap (Forall [] boolType,) $ gatherConsHelper env [(e1,intType), (e2, intType)]
    EIf ec e1 e2 -> do
      newTy <- newType
      cons <- gatherConsHelper env [(ec, boolType), (e1, newTy), (e2, newTy)] 
      return (Forall [] newTy, cons)
    ELet (Name name) e1 e2 -> do
      (t1, c1) <- gatherConstraints env e1
      let substs = unifyAll c1 tmEmpty :: TypeMap
      let newtenv = fmap (generalize env . substTypeScheme substs) env :: TypeEnv
      (t2, c2) <- gatherConstraints (Map.insert name (substTypeScheme substs t1) newtenv) e2
      return (t2, c2)
    EFun (Name name) fexpr -> do
      a <- newType
      (t, c) <- gatherConstraints (Map.insert name (Forall [] a) env) fexpr
      it <- instantiate t
      return (Forall [] $ TFun a it, c)
    ECons car cdr -> do
      (tcar, ccar) <- gatherConstraints env car
      (tcdr, ccdr) <- gatherConstraints env cdr
      itcar <- instantiate tcar
      itcdr <- instantiate tcdr
      return (Forall [] itcdr, (TList itcar `typeEqual` itcdr) : ccar ++ ccdr)
    EApp func arg -> do
      (t1, c1) <- gatherConstraints env func
      (t2, c2) <- gatherConstraints env arg
      a        <- newType
      it1 <- instantiate t1
      it2 <- instantiate t2
      return (Forall [] a, (it1 `typeEqual` TFun it2 a) : c1 ++ c2)
    EMatch mexpr ls -> do
      results <- forM ls (gatherConstraintsPatExpr env)
      (ty, cons) <- gatherConstraints env mexpr
      e <- newType
      ity <- instantiate ty
      return (Forall [] e, concat (List.map ( \ (pty, ety, pcons) -> (ity `typeEqual` pty) : (e `typeEqual` ety) : pcons) results) ++ cons)
    ERLets bindings lexpr -> do
      (newtenv, cons) <- tyRLetBindings env bindings
      (tye, ce) <- gatherConstraints newtenv lexpr
      return (tye, cons ++ ce)
    ENil -> do
      (TVar a) <- newType
      return $ (Forall [a] $ TList (TVar a), [])

gatherConsHelper :: (Monad m, Functor m) => TypeEnv -> [(Expr, Type)] -> St m [TypeCons]

gatherConsHelper tenv ls = fmap List.concat $ sequence $ List.map f ls where
  f (expr, tyEx) = do
    (tyAc, cons) <-  gatherConstraints tenv expr
    ityAc <- instantiate tyAc
    return $ (tyEx `typeEqual` ityAc) : cons

gatherConstraintsPatExpr :: (Monad m, Functor m) => TypeEnv -> (Pat, Expr) -> St m (Type, Type, [TypeCons])
gatherConstraintsPatExpr tenv (pat, expr) = do
  (ty, cons, penv) <- gatherConstraintsPat pat
  (tye, econs) <- gatherConstraints (Map.union penv tenv) expr
  itye <- instantiate tye
  return (ty, itye, econs ++ cons)

gatherConstraintsPat :: (Monad m, Functor m) => Pat -> St m (Type, [TypeCons], TypeEnv)
gatherConstraintsPat (PConst (VInt _)) = return (intType, [], Map.empty)
gatherConstraintsPat (PConst (VBool _)) = return (boolType, [], Map.empty)
gatherConstraintsPat (PConst _      ) = error "(>_<) < weird... invalid const pattern..."
gatherConstraintsPat (PVar (Name name)) = do
  a <- newType
  return (a, [], Map.singleton name (Forall [] a))
gatherConstraintsPat PNil = do
  a <- newType
  return (TList a, [] , Map.empty)
gatherConstraintsPat (PCons pcar pcdr) = do
  (tcar, ccar, ecar) <- gatherConstraintsPat pcar
  (tcdr, ccdr, ecdr) <- gatherConstraintsPat pcdr
  return (tcdr, (TList tcar `typeEqual` tcdr) : ccar ++ ccdr, Map.union ecar ecdr)

tyRLetBindings :: (Monad m, Functor m) => TypeEnv -> [(Name, Name, Expr)] -> St m (TypeEnv, [TypeCons])
tyRLetBindings tenv bindings = do
  defmap <- forM bindings $ \(Name fname, Name vname, fexpr) -> do
    a <- newType
    b <- newType
    return (fname, vname, fexpr, a, b)
  let midmap = List.foldr ( \ (fname, _, _, a, b) -> Map.insert fname (Forall [] $ TFun a b)) tenv defmap
  tcs <- forM defmap $ \ (_, vname, fexpr, a, b) -> do
    (ty, con) <- gatherConstraints (Map.insert vname (Forall [] a) midmap) fexpr
    inst <- instantiate ty
    return $ (b `typeEqual` inst) : con
  return (midmap, concat tcs)

typeInfer :: (Monad m, Functor m) => TypeEnv -> Expr -> St m TypeScheme
typeInfer tenv expr = do
  (ty, cons) <- gatherConstraints tenv expr
  let substs = unifyAll cons tmEmpty
  return $ generalize tenv $ substTypeScheme substs ty

tyRLetBindingsInfer :: (Monad m, Functor m) => TypeEnv -> [(Name, Name, Expr)] -> St m TypeEnv
tyRLetBindingsInfer tenv bindings = do
  (newtenv, cons) <- tyRLetBindings tenv bindings
  let tySubst = unifyAll cons tmEmpty
  return $ fmap (generalize tenv . substTypeScheme tySubst) newtenv

generalize :: TypeEnv -> TypeScheme -> TypeScheme
generalize env tysch@(Forall vars tyy) =
    let free = freeVarsTypeScheme tysch
        wholefree = Map.foldl' (\ f ty -> Set.difference f (freeVarsTypeScheme ty)) free env in
    Forall (Set.toList wholefree ++ vars) tyy

instantiate :: (Monad m, Functor m) => TypeScheme -> St m Type
instantiate (Forall vars ty) = do
    varmap <- forM vars $ \var -> fmap (var,) newType
    return $ runST $ do
      x <- newSTRef ty
      forM_ varmap $ \ (var, cvarty) -> do
          modifySTRef x (subst (Map.singleton var cvarty))
      readSTRef x
