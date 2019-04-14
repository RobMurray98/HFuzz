{-# LANGUAGE PartialTypeSignatures #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  HFuzz.Primitives.List
-- Description :  Expressions for mathematical functions
-- Copyright   :  (c) Robert Murray
-- License     :  MIT
--
-- Maintainer  :  R.Murray.1@warwick.ac.uk
-- Stability   :  experimental
--
-- Module providing expressions for mathematical functions.
--
-----------------------------------------------------------------------------

module HFuzz.Primitives.Math (
    curriedAdd,
    curriedMinus,
    curriedTimes,
    add,
    sub,
    times,
    scale,
    scaleInt,
    fcurriedAdd,
    fadd,
    fcurriedSub,
    fsub,
    fscale,
    fscaleInt,
    fcurriedTimes,
    ftimes,
    xsum
    ) where


import HFuzz.Internal.Types.Sens
import HFuzz.Internal.Types.Expr
import HFuzz.Internal.Types.Ty
import HFuzz.Internal.Types.Context
import HFuzz.Primitives.List
import Data.TypeNums
import Data.Proxy

curriedAdd = XPlusCurried
curriedMinus = XMinusCurried
curriedTimes = XTimesCurried
add = XPlus
sub = XMinus
times = XTimes
scale :: KnownRat r => Proxy r -> _
scale = XScaleNum
scaleInt :: KnownInt i => Proxy i -> _
scaleInt = XScaleInt

-- 1-sensitive curried addition function
fcurriedAdd :: Expr _ _ (TLolli _ _ (TLolli _ _ _))
fcurriedAdd = XAbs (Var @"x") (XAbs (Var @"y") (XPlusCurried (XVar (Var @"x")) (XVar (Var @"y"))))

-- 1-sensitive curried subtraction function
fcurriedSub :: Expr _ _ (TLolli _ _ (TLolli _ _ _))
fcurriedSub = XAbs (Var @"x") (XAbs (Var @"y") (XMinusCurried (XVar (Var @"x")) (XVar (Var @"y"))))

-- inf-sensitive curried multiplication function
fcurriedTimes :: Expr _ _ (TLolli _ _ (TLolli _ _ _))
fcurriedTimes = XAbs (Var @"x") (XAbs (Var @"y") (XTimesCurried (XVar (Var @"x")) (XVar (Var @"y"))))

-- 1-sensitive uncurried addition function
fadd :: Expr _ _ (TLolli (TPrim (PrimTens pt pt)) _ (TPrim pt))
fadd = XAbs (Var @"x") (XPlus (XVar (Var @"x")))

-- 1-sensitive uncurried subtraction function
fsub :: Expr _ _ (TLolli (TPrim (PrimTens pt pt)) _ (TPrim pt))
fsub = XAbs (Var @"x") (XMinus (XVar (Var @"x")))

-- inf-sensitive uncurried multiplication function
ftimes :: Expr _ _ (TLolli (TPrim (PrimTens pt pt)) _ (TPrim pt))
ftimes = XAbs (Var @"x") (XTimes (XVar (Var @"x")))

-- r-sensitive function that scales a numeric value by some rational r
fscale :: KnownRat r => Proxy r -> Expr _ _ _
fscale pr = XAbs (Var @"x") (XScaleNum pr (XVar (Var @"x")))

-- i-sensitive function that scales a numeric value by some integer i
fscaleInt :: KnownInt i => Proxy i -> Expr _ _ _
fscaleInt pr = XAbs (Var @"x") (XScaleInt pr (XVar (Var @"x")))

-- TODO: without specifying start context will not compile!
--       also doesn't work using deferred type errors!
-- 1-sensitive sum function on a list of numeric values
xsum :: Expr '[] _ _
xsum = XAbs (Var @"z") (XFoldl fadd (XCNum (Proxy @(Pos 0 :% 1))) (XVar (Var @"z")))