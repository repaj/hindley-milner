{-# LANGUAGE DeriveDataTypeable #-}
module Tree
  ( Term (..)
  , Type (..)
  , Schema (..)
  , Name
  , FV (..)
  )
where

import Data.Generics
import Data.Set (Set)
import qualified Data.Set as Set

type Name = String

data Term
  = TmVar Name
  | TmAbs Name Term
  | TmApp Term Term
  | TmLet Name Term Term
  | TmInt Integer
  | TmBool Bool
  deriving (Eq, Show, Data, Typeable)

data Type
  = TyVar Name
  | TyFun Type Type
  | TyInt
  | TyBool
  deriving (Eq, Data, Typeable)

instance Show Type where
  show (TyVar a) = a
  show (TyFun a b) = "(" ++ show a ++ " -> " ++ show b ++ ")"
  show TyInt = "Int"
  show TyBool = "Bool"

data Schema
  = Schema [Name] Type
  deriving (Eq, Data, Typeable)

instance Show Schema where
  show (Schema [] t) = show t
  show (Schema vs t) = "(forall " ++ unwords vs ++ " . " ++ show t ++ ")"

class FV a where
  fv :: a -> Set Name

instance FV Term where
  fv (TmVar x)     = Set.singleton x
  fv (TmAbs x t)   = x `Set.delete` fv t
  fv (TmApp t1 t2) = fv t1 `Set.union` fv t2
  fv _             = Set.empty

instance FV Type where
  fv = everything Set.union (mkQ Set.empty go)
    where
      go (TyVar a) = Set.singleton a

instance FV Schema where
  fv (Schema as t) = foldr Set.delete (fv t) as
