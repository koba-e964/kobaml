{-# LANGUAGE BangPatterns, TupleSections #-}
module TypeInf where

import Control.Exception
import Control.Monad.State
import Control.Monad.ST
import Control.Monad.Error
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.List as List
import Data.STRef
import Prelude hiding (map)

import CDef


type St m = ErrorT TypeError (StateT Int m) -- the state of the type inferrer

addCons :: TypeVar -> Type -> (TypeSubst, [TypeCons]) -> (TypeSubst, [TypeCons])
addCons var ty (tmap, cons) =
  let inc = subst (Map.singleton var ty)
      conv = Map.insert var ty $ Map.map inc tmap
      newcons = List.map (\(TypeEqual ty1 ty2) -> inc ty1 `typeEqual` inc ty2) cons in
    (conv, newcons)

unifyAll :: [TypeCons] -> TypeSubst
unifyAll cons = sub tmEmpty cons where
         sub m [] = m
         sub m ((TypeEqual ty1 ty2):rest) = let (newm, newrest) = unify ty1 ty2 (m, rest) in sub newm newrest


unify :: Type -> Type -> (TypeSubst, [TypeCons]) -> (TypeSubst, [TypeCons])

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
unify (TPair a1 b1) (TPair a2 b2) (map, cons) =
  (map, (a1 `typeEqual` a2) : (b1 `typeEqual` b2) : cons)
unify x y _ = unifyError x y

unifyError :: Type -> Type -> a
unifyError x y = throw $ TypeError $ "Cannot unify " ++ show x ++" with " ++ show y ++ " (T_T)"

errorRecursive :: TypeVar -> Type -> (TypeSubst, [TypeCons]) -> a
errorRecursive tvar ty mapc = throw $ TypeError $ "Cannot construct recursive type(>_<) :" ++ show tvar ++ " = " ++ show ty ++ " in " ++ show mapc


subst :: TypeSubst -> Type -> Type
subst tmap (TVar v) =
  case Map.lookup v tmap of
    Just t -> t
    Nothing -> TVar v
subst tmap (TFun x y) = TFun (subst tmap x) (subst tmap y)
subst _    (TConc x)  = TConc x
subst tmap (TList a) = TList (subst tmap a)
subst tmap (TPair x y) = TPair (subst tmap x) (subst tmap y)

substTypeScheme :: TypeSubst -> TypeScheme -> TypeScheme
substTypeScheme tmap (Forall vars ty) =
  let newtmap = Set.foldl' (\m x -> Map.delete x m) tmap vars in
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

gatherConstraints :: (Monad m, Functor m) => TypeEnv -> Expr -> St m (Type, [TypeCons])

gatherConstraints !env !expr =
  case expr of
    EConst val -> case val of
      VInt _  -> return (intType, [])
      VBool _ -> return (boolType, [])
      _       -> undefined
    EVar (Name name) -> case Map.lookup name env of
      Just ty -> fmap (, []) $ instantiate ty
      Nothing -> throw $ TypeError $ "unbound variable: " ++ name
    EAdd e1 e2 -> fmap (intType,) $ gatherConsHelper env [(e1,intType), (e2, intType)]
    ESub e1 e2 -> fmap (intType,) $ gatherConsHelper env [(e1,intType), (e2, intType)]
    EMul e1 e2 -> fmap (intType,) $ gatherConsHelper env [(e1,intType), (e2, intType)]
    EDiv e1 e2 -> fmap (intType,) $ gatherConsHelper env [(e1,intType), (e2, intType)]
    ELt  e1 e2 -> fmap (boolType,) $ gatherConsHelper env [(e1,intType), (e2, intType)]
    EEq  e1 e2 -> fmap (boolType,) $ gatherConsHelper env [(e1,intType), (e2, intType)]
    EIf ec e1 e2 -> do
      newTy <- newType
      cons <- gatherConsHelper env [(ec, boolType), (e1, newTy), (e2, newTy)] 
      return (newTy, cons)
    ELet (Name name) e1 e2 -> do
      (t1, c1) <- gatherConstraints env e1
      let substs = unifyAll c1:: TypeSubst
      let etysch = generalize env $ subst substs t1
      let newtenv = fmap (generalizeTypeScheme env . substTypeScheme substs) env :: TypeEnv
      (t2, c2) <- gatherConstraints (Map.insert name etysch newtenv) e2
      return (t2, c1 ++ c2)
    EFun (Name name) fexpr -> do
      a <- newType
      (t, c) <- gatherConstraints (Map.insert name (fromType a) env) fexpr
      let substs = unifyAll c
      let ty = subst substs $ TFun a t
      return (ty, c)
    ECons car cdr -> do
      (tcar, ccar) <- gatherConstraints env car
      (tcdr, ccdr) <- gatherConstraints env cdr
      return (tcdr, (TList tcar `typeEqual` tcdr) : ccar ++ ccdr)
    EApp func arg -> do
      (t1, c1) <- gatherConstraints env func
      (t2, c2) <- gatherConstraints env arg
      a        <- newType
      return (a, (t1 `typeEqual` TFun t2 a) : c1 ++ c2)
    EMatch mexpr ls -> do
      results <- forM ls (gatherConstraintsPatExpr env)
      (ty, cons) <- gatherConstraints env mexpr
      e <- newType
      return (e, concat (List.map ( \ (pty, ety, pcons) -> (ty `typeEqual` pty) : (e `typeEqual` ety) : pcons) results) ++ cons)
    ERLets bindings lexpr -> do
      (newtenv, cons) <- tyRLetBindings env bindings
      (tye, ce) <- gatherConstraints newtenv lexpr
      return (tye, cons ++ ce)
    EPair efst esnd -> do
      (tfst, cfst) <- gatherConstraints env efst
      (tsnd, csnd) <- gatherConstraints env esnd
      return (TPair tfst tsnd, cfst ++ csnd)
    ENil -> do
      a <- newType
      return $ (TList a, [])

gatherConsHelper :: (Monad m, Functor m) => TypeEnv -> [(Expr, Type)] -> St m [TypeCons]

gatherConsHelper tenv ls = fmap List.concat $ sequence $ List.map f ls where
  f (expr, tyEx) = do
    (tyAc, cons) <-  gatherConstraints tenv expr
    return $ (tyEx `typeEqual` tyAc) : cons

gatherConstraintsPatExpr :: (Monad m, Functor m) => TypeEnv -> (Pat, Expr) -> St m (Type, Type, [TypeCons])
gatherConstraintsPatExpr tenv (pat, expr) = do
  (ty, cons, penv) <- gatherConstraintsPat pat
  (tye, econs) <- gatherConstraints (Map.union penv tenv) expr
  return (ty, tye, econs ++ cons)

gatherConstraintsPat :: (Monad m, Functor m) => Pat -> St m (Type, [TypeCons], TypeEnv)
gatherConstraintsPat (PConst (VInt _)) = return (intType, [], Map.empty)
gatherConstraintsPat (PConst (VBool _)) = return (boolType, [], Map.empty)
gatherConstraintsPat (PConst _      ) = error "(>_<) < weird... invalid const pattern..."
gatherConstraintsPat (PVar (Name name)) = do
  a <- newType
  return (a, [], Map.singleton name (fromType a))
gatherConstraintsPat PNil = do
  a <- newType
  return (TList a, [] , Map.empty)
gatherConstraintsPat (PCons pcar pcdr) = do
  (tcar, ccar, ecar) <- gatherConstraintsPat pcar
  (tcdr, ccdr, ecdr) <- gatherConstraintsPat pcdr
  return (tcdr, (TList tcar `typeEqual` tcdr) : ccar ++ ccdr, Map.union ecar ecdr)
gatherConstraintsPat (PPair pcar pcdr) = do
  (tcar, ccar, ecar) <- gatherConstraintsPat pcar
  (tcdr, ccdr, ecdr) <- gatherConstraintsPat pcdr
  return (TPair tcar tcdr, ccar ++ ccdr, Map.union ecar ecdr)

tyRLetBindings :: (Monad m, Functor m) => TypeEnv -> [(Name, Expr)] -> St m (TypeEnv, [TypeCons])
tyRLetBindings tenv bindings = do
  defmap <- forM bindings $ \(Name fname, fexpr) -> do
    a <- newType
    return (fname, fexpr, a)
  let midmap = List.foldr ( \ (fname, _, a) -> Map.insert fname (fromType $ a)) tenv defmap
  tcs <- forM defmap $ \ (_, fexpr, a) -> do
    (ty, con) <- gatherConstraints midmap fexpr
    return $ (a `typeEqual` ty) : con
  return (midmap, concat tcs)

typeInfer :: (Monad m, Functor m) => TypeEnv -> Expr -> St m TypeScheme
typeInfer !tenv !expr = do
  (!ty, !cons) <- gatherConstraints tenv expr
  let !substs = unifyAll cons
  return $ generalize tenv $ subst substs ty

tyRLetBindingsInfer :: (Monad m, Functor m) => TypeEnv -> [(Name, Expr)] -> St m TypeEnv
tyRLetBindingsInfer tenv bindings = do
  (newtenv, cons) <- tyRLetBindings tenv bindings
  let tySubst = unifyAll cons
  return $ fmap (generalizeTypeScheme tenv . substTypeScheme tySubst) newtenv

generalize :: TypeEnv -> Type -> TypeScheme
generalize !tenv !ty = generalizeTypeScheme tenv (fromType ty)

generalizeTypeScheme :: TypeEnv -> TypeScheme -> TypeScheme
generalizeTypeScheme env tysch@(Forall vars tyy) =
    let varlist = List.map TypeVar $ List.map (:[]) ['a'..'z'] ++ List.map (\x -> "t" ++ show x) [(0 :: Integer)..]
        free = freeVarsTypeScheme tysch
        wholefree = Map.foldl' (\ f ty -> Set.difference f (freeVarsTypeScheme ty)) free env
        freeEnv = Map.foldl' (\f ty -> Set.union f (freeVarsTypeScheme ty)) Set.empty env
        forbidden = Set.union (freeVars tyy) freeEnv
        replace (s, ty) v= let Just newv = List.find (\x -> Set.notMember x forbidden && Set.notMember x s) varlist in (Set.insert newv s, subst (Map.singleton v (TVar newv)) ty)
        (ss, tyty) = Set.foldl' replace (vars, tyy) wholefree
      in
    Forall ss tyty

instantiate :: (Monad m, Functor m) => TypeScheme -> St m Type
instantiate (Forall vars ty) = do
    varmap <- forM (Set.toList vars) $ \var -> fmap (var,) newType
    return $ runST $ do
      x <- newSTRef ty
      forM_ varmap $ \ (var, cvarty) -> do
          modifySTRef x (subst (Map.singleton var cvarty))
      readSTRef x
