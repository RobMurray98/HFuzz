-----------------------------------------------------------------------------
-- |
-- Module      :  HFuzz.Primitives.Boolean
-- Description :  Expressions for boolean operations
-- Copyright   :  (c) Robert Murray
-- License     :  MIT
--
-- Maintainer  :  R.Murray.1@warwick.ac.uk
-- Stability   :  experimental
--
-- Module providing expressions for describing boolean operations.
--
-----------------------------------------------------------------------------

module HFuzz.Primitives.Boolean (
    band,
    bor,
    bnot,
    fand,
    for,
    fnot,
    -- fuand,
    -- fuor
    ) where

import HFuzz.Internal.Types.Expr

-- inf-sensitive curried boolean and function
fand = XAbs (Var @"x") (XAbs (Var @"y") (XAnd (XVar (Var @"x")) (XVar (Var @"y"))))
-- inf-sensitive curried boolean or function
for = XAbs (Var @"x") (XAbs (Var @"y") (XOr (XVar (Var @"x")) (XVar (Var @"y"))))
-- inf-sensitive boolean not function
fnot = XAbs (Var @"x") (XNot (XVar (Var @"x")))
-- -- inf-sensitive uncurried boolean and function
-- fuand = XAbs (Var @"x") (XLet (Var @"a") (Var @"b") (XVar (Var @"x")) (XAnd (XVar (Var @"a")) (XVar (Var @"b"))))
-- -- inf-sensitive uncurried boolean or function
-- fuor = XAbs (Var @"x") (XLet (Var @"a") (Var @"b") (XVar (Var @"x")) (XOr (XVar (Var @"a")) (XVar (Var @"b"))))

-- boolean and
band = XAnd
-- boolean or
bor = XOr
-- boolean not
bnot = XNot