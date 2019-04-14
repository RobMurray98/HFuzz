{-# LANGUAGE UndecidableInstances #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  HFuzz.Internal.Types.Ty
-- Description :  Types of expressions
-- Copyright   :  (c) Robert Murray
-- License     :  MIT
--
-- Maintainer  :  R.Murray.1@warwick.ac.uk
-- Stability   :  experimental
--
-- Module providing data types for the permitted types of expressions.
--
-----------------------------------------------------------------------------

module HFuzz.Internal.Types.Ty (
    Ty(..),
    BaseTy(..),
    PrimTy(..),
    SPrimTy(..),
    Unit(..),
    TyOfPrimTy
    ) where

import HFuzz.Internal.Types.Sens

import qualified Data.TypeNums as TN
import Data.TypeNums(TInt(..), Rat(..))
import Data.Type.Bool
import GHC.TypeLits

-- unit type
data Unit = Unit

-- get the type of a base type
type family TyOfBaseTy (b :: BaseTy) :: * where
    TyOfBaseTy BInt = Int
    TyOfBaseTy BNum = Double
    TyOfBaseTy BUnit = Unit
    TyOfBaseTy (BBool _) = Bool
    TyOfBaseTy (BString _) = String

-- get the type of a primitive type
type family TyOfPrimTy (p :: PrimTy) :: * where
    TyOfPrimTy (PrimList _ pt) = [TyOfPrimTy pt]
    TyOfPrimTy (Prim b) = TyOfBaseTy b

data SPrimTy (pt :: PrimTy) where
    SPrimTy :: SPrimTy pt

-- The base types available
data BaseTy where
    BInt :: BaseTy
    BNum :: BaseTy
    BUnit :: BaseTy
    BBool :: Maybe Bool -> BaseTy
    BString :: Maybe Symbol -> BaseTy

-- The primitive types available
data PrimTy where
    PrimList :: Nat -> PrimTy -> PrimTy
    PrimTens :: PrimTy -> PrimTy -> PrimTy -- pair type which is eliminated by pattern matching
    PrimAmp :: PrimTy -> PrimTy -> PrimTy -- pair type which is eliminated by HFuzzion
    Prim :: BaseTy -> PrimTy

-- The types available - either a sensitivity-annotated function or a primitive type
data Ty where
    TPrim :: PrimTy -> Ty -- primitive type
    TLolli :: Ty -> Sens -> Ty -> Ty -- sensitivity-annotated function type