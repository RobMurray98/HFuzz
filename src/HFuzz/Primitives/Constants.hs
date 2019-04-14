{-# LANGUAGE PartialTypeSignatures #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  HFuzz.Primitives.Constants
-- Description :  Expressions for constant values
-- Copyright   :  (c) Robert Murray
-- License     :  MIT
--
-- Maintainer  :  R.Murray.1@warwick.ac.uk
-- Stability   :  experimental
--
-- Module providing expressions for describing constant values.
--
-----------------------------------------------------------------------------

module HFuzz.Primitives.Constants (
    true,
    false,
    unit,
    int,
    num,
    bool,
    emptyList
    ) where

import HFuzz.Internal.Data.Bool

import HFuzz.Internal.Types.Sens
import HFuzz.Internal.Types.Expr
import HFuzz.Internal.Types.Ty
import HFuzz.Internal.Types.Context
import Data.Proxy
import Data.TypeNums

true = XCTrue
false = XCFalse
unit = XCUnit
int :: KnownInt i => Proxy i -> Expr _ _ _
int = XCInt
num :: KnownRat r => Proxy r -> Expr _ _ _
num = XCNum
bool :: KnownBool b => Proxy b -> Expr _ _ _
bool = XCBool
emptyList = XCEmptyList