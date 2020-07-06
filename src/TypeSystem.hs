{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleContexts #-}
module TypeSystem where

import Tree
import Subst

import Control.Monad.Except
import Control.Monad.Writer
import Control.Monad.RWS
import Data.List (foldl')
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Generics
import qualified Data.Set as Set
import GHC.Exts (Item)
import qualified GHC.Exts as Ext

newtype Context
  = Context { unContext :: Map Name Schema }
  deriving (Eq, Show, Data, Typeable)

emptyContext :: Context
emptyContext = Context Map.empty

instance Ext.IsList Context where
  type Item Context = (Name, Schema)
  fromList = Context . Map.fromList
  toList = Map.toList . unContext

instance Semigroup Context where
  (Context m1) <> (Context m2) = Context (m1 `Map.union` m2)

instance Monoid Context where
  mempty = emptyContext

instance FV Context where
  fv (Context ctx) = Map.foldr Set.union Set.empty . Map.map fv $ ctx

instance CanSubst Context where
  applySubst s (Context m) = Context (Map.map (applySubst s) m)

data Constraint = Type :~: Type deriving (Eq, Show, Data, Typeable)

instance CanSubst Constraint where
  applySubst s (t1 :~: t2) = applySubst s t1 :~: applySubst s t2

type ConstraintSet = [Constraint]

data UnifyError
  = InfiniteType Name Type
  | CannotUnify Constraint
  deriving (Eq, Show)

unify :: Constraint -> Either UnifyError Subst
unify (t1 :~: t2) | t1 == t2 = pure emptySubst
unify (t :~: TyVar a) = bind a t
unify (TyVar a :~: t) = bind a t
unify (TyFun a b :~: TyFun c d) = solve [a :~: c, b :~: d]
unify c = Left . CannotUnify $ c

bind :: Name -> Type -> Either UnifyError Subst
bind a t | a `Set.member` fv t = Left (InfiniteType a t)
         | otherwise = Right . Subst $ [(a, t)]


solve :: ConstraintSet -> Either UnifyError Subst
solve = foldl' go (Right emptySubst)
  where
    go acc c = do
      s  <- acc
      s' <- unify (applySubst s c)
      pure (s' <> s)

data TypeError
  = UnboundType Name
  | UnifyError UnifyError
  | CannotInfer Term
  deriving (Show)

newtype TypeCheck a = TypeCheck { unTypeCheck :: RWST Context [Constraint] [Name] (Except TypeError) a }
  deriving ( Functor
           , Applicative
           , Monad
           , MonadReader Context
           , MonadWriter [Constraint]
           , MonadState [Name]
           , MonadError TypeError
           )

fresh :: TypeCheck Name
fresh = do
  v <- gets head
  modify tail
  ctx <- ask
  if v `Set.member` fv ctx
    then fresh
    else return v

lookupCtx :: Name -> TypeCheck Schema
lookupCtx name = maybe (throwError (UnboundType name)) pure . Map.lookup name . unContext =<< ask

instantiate :: Schema -> TypeCheck Type
instantiate (Schema vs t) = do
  vs' <- mapM (const fresh) vs
  let subst = Subst . Map.fromList $ vs `zip` map TyVar vs'
  return (applySubst subst t)

generalize :: MonadReader Context m => Type -> m Schema
generalize t = do
  ctx <- ask
  let vs = Set.toList $ fv t `Set.difference` fv ctx
  pure (Schema vs t)

infer :: Term -> TypeCheck Type
infer (TmInt _) = pure TyInt
infer (TmBool _) = pure TyBool
infer (TmVar x) = instantiate =<< lookupCtx x
infer (TmApp t1 t2) = do
  t1'      <- infer t1
  t2'      <- infer t2
  f        <- TyVar <$> fresh
  tell [t1' :~: TyFun t2' f]
  pure f
infer (TmAbs x t) = do
  tx <- TyVar <$> fresh
  t' <- local (<> [(x, Schema [] tx)]) $ infer t
  pure (TyFun tx t')
infer (TmLet x e t) = do
  (te, cs) <- listen (infer e)
  s <- either (throwError . UnifyError) pure (solve cs)
  gte <- local (applySubst s) (generalize (applySubst s te))
  tt <- local ((<> [(x, gte)]) . applySubst s) (infer t)
  pure tt

runTypeCheck :: TypeCheck a -> Context -> Either TypeError (a, [Constraint])
runTypeCheck tc ctx = runExcept $ do
  (a, _, s) <- runRWST (unTypeCheck tc) ctx freshVars
  pure (a, s)
  where
    freshVars = concat [replicateM i ['a'..'z'] | i <- [1..]]

runInfer :: Context -> Term -> Either TypeError Schema
runInfer ctx tm = do
  (ty, cs) <- runTypeCheck (infer tm) ctx
  s <- either (throwError . UnifyError) pure (solve cs)
  pure (generalize (applySubst s ty) ctx)
