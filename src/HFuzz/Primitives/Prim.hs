{-# LANGUAGE PartialTypeSignatures #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  HFuzz.Primitives.Prim
-- Description :  Expressions for primitive operations
-- Copyright   :  (c) Robert Murray
-- License     :  MIT
--
-- Maintainer  :  R.Murray.1@warwick.ac.uk
-- Stability   :  experimental
--
-- Module providing expressions for primitive operations such as
-- abstraction, application and variable reference.
--
-----------------------------------------------------------------------------

module HFuzz.Primitives.Prim (
    ref,
    Var,
    xid,
    apply,
    apply2,
    apply3,
    abstract,
    abstractInf,
    mkPairT,
    mkPairA,
    swapT,
    swapA,
    xlet,
    xfst,
    xsnd,
    ifelse
    ) where


import HFuzz.Internal.Types.Sens
import HFuzz.Internal.Types.Expr
import HFuzz.Internal.Types.Ty
import HFuzz.Internal.Types.Context
import GHC.TypeLits

-- variable reference
ref :: KnownSymbol x => Var x t -> Expr _ _ _
ref = XVar
-- function application
apply = XApp
-- function abstraction
abstract :: KnownSymbol x => Var x t -> Expr _ _ _ -> Expr _ _ _
abstract = XAbs
-- unbounded function abstraction
abstractInf :: KnownSymbol x => Var x t -> Expr _ _ _ -> Expr _ _ _
abstractInf = XAbsInf
-- tensor pair introduction
mkPairT = XPairT
-- & pair introduction
mkPairA = XPairA
-- swap values of a tensor pair
swapT = XSwapT
-- swap values of a & pair
swapA = XSwapA
-- let statement (tensor pair elimination)
xlet :: (KnownSymbol x, KnownSymbol y) => Var x _ -> Var y _ -> Expr _ _ _ -> Expr _ _ _ -> Expr _ _ _
xlet = XLet
-- first element of pair (& pair elimination by projection)
xfst = XFstA
-- second element of pair (& pair elimination by projection)
xsnd = XSndA
-- conditional statement
ifelse = XIfElse
-- identity function
xid = abstract (Var @"id") (ref (Var @"id"))
-- apply f to x, then apply to y
apply2 f x y = apply (apply f x) y
-- apply f to x, then apply to y, then apply to z
apply3 f x y z = apply (apply (apply f x) y) z