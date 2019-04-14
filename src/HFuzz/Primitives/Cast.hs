-----------------------------------------------------------------------------
-- |
-- Module      :  HFuzz.Primitives.Cast
-- Description :  Expressions for type casting operations
-- Copyright   :  (c) Robert Murray
-- License     :  MIT
--
-- Maintainer  :  R.Murray.1@warwick.ac.uk
-- Stability   :  experimental
--
-- Module providing expressions for describing type casting operations.
--
-----------------------------------------------------------------------------

module HFuzz.Primitives.Cast (
    fcastIB,
    fcastNB,
    fcastIN,
    fcastBI,
    fcastBN,
    fcastNI,
    castIB,
    castNB,
    castIN,
    castBI,
    castBN,
    castNI
    ) where

import HFuzz.Internal.Types.Expr

-- inf-sensitive type cast function from int to bool
fcastIB = XAbs (Var @"x") (XCastIntBool (XVar (Var @"x")))
-- inf-sensitive type cast function from num to bool
fcastNB = XAbs (Var @"x") (XCastNumBool (XVar (Var @"x")))
-- 1-sensitive type cast function from int to num
fcastIN = XAbs (Var @"x") (XCastIntNum (XVar (Var @"x")))
-- 1-sensitive type cast function from bool to int
fcastBI = XAbs (Var @"x") (XCastBoolInt (XVar (Var @"x")))
-- 1-sensitive type cast function from bool to num
fcastBN = XAbs (Var @"x") (XCastBoolNum (XVar (Var @"x")))
-- inf-sensitive type cast function from num to int
fcastNI = XAbs (Var @"x") (XCastNumInt (XVar (Var @"x")))

-- int to bool
castIB = XCastIntBool
-- num to bool
castNB = XCastNumBool
-- int to num
castIN = XCastIntNum
-- bool to int
castBI = XCastBoolInt
-- bool to num
castBN = XCastBoolNum
-- num to int
castNI = XCastNumInt