{-# LANGUAGE TypeFamilies #-}
module Subst
  ( Subst (..)
  , emptySubst
  , composeSubst
  , CanSubst (..)
  )
where

import           Tree

import           Data.Generics
import           Data.Map (Map)
import qualified Data.Map as Map
import           GHC.Exts (Item)
import qualified GHC.Exts as Ext
  
newtype Subst
  = Subst { unSubst :: Map Name Type }
  deriving (Show)

instance Ext.IsList Subst where
  type Item Subst = (Name, Type)
  fromList = Subst . Map.fromList
  toList = Map.toList . unSubst

emptySubst :: Subst
emptySubst = Subst Map.empty

composeSubst :: Subst -> Subst -> Subst
composeSubst s@(Subst s1) (Subst s2) = Subst (Map.map (applySubst s) s2 `Map.union` s1)

instance Semigroup Subst where
  (<>) = composeSubst

instance Monoid Subst where
  mempty = emptySubst

class CanSubst a where
  applySubst :: Subst -> a -> a

instance CanSubst Type where
  applySubst (Subst m) = everywhere (mkT go)
    where
      go (TyVar a) = Map.findWithDefault (TyVar a) a m
      go oth = oth

instance CanSubst Schema where
  applySubst (Subst m) (Schema as t) = Schema as (applySubst s' t)
    where
      s' = Subst (foldr Map.delete m as)
