{-# LANGUAGE PartialTypeSignatures #-}
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
    xeq,
    xgt,
    xlt,
    xgte,
    xlte
    ) where

import HFuzz.Internal.Types.Expr

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
fueq = XAbs (Var @"x") (XLet (Var @"a") (Var @"b") (XVar (Var @"x")) (XEQ (XVar (Var @"a")) (XVar (Var @"b"))))
-- inf-sensitive uncurried greater-than comparison function
fugt = XAbs (Var @"x") (XLet (Var @"a") (Var @"b") (XVar (Var @"x")) (XGT (XVar (Var @"a")) (XVar (Var @"b"))))
-- inf-sensitive uncurried less-than comparison function
fult = XAbs (Var @"x") (XLet (Var @"a") (Var @"b") (XVar (Var @"x")) (XLT (XVar (Var @"a")) (XVar (Var @"b"))))
-- inf-sensitive uncurried greater-than-or-equal comparison function
fugte = XAbs (Var @"x") (XLet (Var @"a") (Var @"b") (XVar (Var @"x")) (XGTE (XVar (Var @"a")) (XVar (Var @"b"))))
-- inf-sensitive uncurried less-than-or-equal comparison function
fulte = XAbs (Var @"x") (XLet (Var @"a") (Var @"b") (XVar (Var @"x")) (XLTE (XVar (Var @"a")) (XVar (Var @"b"))))

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