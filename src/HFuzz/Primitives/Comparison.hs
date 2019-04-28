{-# OPTIONS_GHC -fdefer-type-errors #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  HFuzz.Primitives.Comparison
-- Description :  Expressions for comparison operations
-- Copyright   :  (c) Robert Murray
-- License     :  MIT
--
-- Maintainer  :  R.Murray.1@warwick.ac.uk
-- Stability   :  experimental
--
-- Module providing expressions for describing comparison operations.
--
-----------------------------------------------------------------------------

module HFuzz.Primitives.Comparison (
    feq,
    fgt,
    flt,
    fgte,
    flte,
    fueq,
    fugt,
    fult,
    fugte,
    fulte,
    cswp,
    xeq,
    xgt,
    xlt,
    xgte,
    xlte
    ) where

import HFuzz.Internal.Types.Expr
import HFuzz.Internal.Types.Ty

-- inf-sensitive curried equality comparison function
feq = XAbs (Var @"x") (XAbs (Var @"y") (XEQ (XVar (Var @"x")) (XVar (Var @"y"))))
-- inf-sensitive curried greater-than comparison function
fgt = XAbs (Var @"x") (XAbs (Var @"y") (XGT (XVar (Var @"x")) (XVar (Var @"y"))))
-- inf-sensitive curried less-than comparison function
flt = XAbs (Var @"x") (XAbs (Var @"y") (XLT (XVar (Var @"x")) (XVar (Var @"y"))))
-- inf-sensitive curried greater-than-or-equal comparison function
fgte = XAbs (Var @"x") (XAbs (Var @"y") (XGTE (XVar (Var @"x")) (XVar (Var @"y"))))
-- inf-sensitive curried less-than-or-equal comparison function
flte = XAbs (Var @"x") (XAbs (Var @"y") (XLTE (XVar (Var @"x")) (XVar (Var @"y"))))


-- TODO: can we compile these without deferred type errors?

-- inf-sensitive uncurried equality comparison function
fueq = XAbs (Var @"x" @(PrimTens _ _)) (XLet (Var @"a") (Var @"b") (XVar (Var @"x")) (XEQ (XVar (Var @"a")) (XVar (Var @"b"))))
-- inf-sensitive uncurried greater-than comparison function
fugt = XAbs (Var @"x" @(PrimTens _ _)) (XLet (Var @"a") (Var @"b") (XVar (Var @"x")) (XGT (XVar (Var @"a")) (XVar (Var @"b"))))
-- inf-sensitive uncurried less-than comparison function
fult = XAbs (Var @"x" @(PrimTens _ _)) (XLet (Var @"a") (Var @"b") (XVar (Var @"x")) (XLT (XVar (Var @"a")) (XVar (Var @"b"))))
-- inf-sensitive uncurried greater-than-or-equal comparison function
fugte = XAbs (Var @"x" @(PrimTens _ _)) (XLet (Var @"a") (Var @"b") (XVar (Var @"x")) (XGTE (XVar (Var @"a")) (XVar (Var @"b"))))
-- inf-sensitive uncurried less-than-or-equal comparison function
fulte = XAbs (Var @"x" @(PrimTens _ _)) (XLet (Var @"a") (Var @"b") (XVar (Var @"x")) (XLTE (XVar (Var @"a")) (XVar (Var @"b"))))
-- 1-sensitive function that swaps the values in a pair if the second is less than the first
cswp = XAbs (Var @"p" @(PrimTens _ _)) (XIfElse (XLet (Var @"a") (Var @"b") (XVar (Var @"p")) (XLT (XVar (Var @"a")) (XVar (Var @"b")))) (XVar (Var @"p")) (XSwapT (XVar (Var @"p"))))

-- (==)
xeq = XEQ
-- (>)
xgt = XGT
-- (<)
xlt = XLT
-- (>=)
xgte = XGTE
-- (<=)
xlte = XLTE