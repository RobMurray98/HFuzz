{-# LANGUAGE PartialTypeSignatures #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  HFuzz.Primitives.List
-- Description :  Expressions for lists and operations on lists
-- Copyright   :  (c) Robert Murray
-- License     :  MIT
--
-- Maintainer  :  R.Murray.1@warwick.ac.uk
-- Stability   :  experimental
--
-- Module providing expressions for describing lists and operations over
-- lists such as maps and folds.
--
-----------------------------------------------------------------------------

module HFuzz.Primitives.List (
    lcons,
    lhead,
    fcons,
    fhead,
    listOfElem,
    lfoldl,
    lfoldr,
    lfoldl1,
    lfoldr1,
    lmap
    ) where

import HFuzz.Internal.Types.Sens
import HFuzz.Internal.Types.Expr
import HFuzz.Internal.Types.Ty
import qualified Data.TypeNums as TN

lcons = XCons
lhead :: forall n. ((TN.>=) n 1) => Expr _ _ (TPrim (PrimList n _)) -> _
lhead = XHead

-- 1-sensitive cons function
fcons :: forall n pt. Expr _ _ (TLolli (TPrim pt) _ (TLolli (TPrim (PrimList n pt)) _ (TPrim (PrimList ((TN.+) n 1) pt))))
fcons = XAbs (Var @"a") (XAbs (Var @"as") (XCons (XVar (Var @"a")) (XVar (Var @"as"))))

-- 1-sensitive head function
fhead :: forall n pt. ((TN.>=) n 1) => Expr _ _ (TLolli (TPrim (PrimList n pt)) _ (TPrim (PrimTens pt (PrimList ((TN.-) n 1) pt))))
fhead = XAbs (Var @"as") (XHead (XVar (Var @"as")))

listOfElem = XCListOfElem

lfoldl = XFoldl
lfoldr = XFoldr
lfoldl1 :: forall n. ((TN.>=) n 1) => _ -> Expr _ _ (TPrim (PrimList n _)) -> _
lfoldl1 = XFoldl1
lfoldr1 :: forall n. ((TN.>=) n 1) => _ -> Expr _ _ (TPrim (PrimList n _)) -> _
lfoldr1 = XFoldr1
lmap = XMap