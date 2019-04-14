{-# OPTIONS_GHC -fdefer-type-errors #-}
{-# LANGUAGE PartialTypeSignatures #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  HFuzz.Types.ExprSpec
-- Description :  Test cases for expressions
-- Copyright   :  (c) Robert Murray
-- License     :  MIT
--
-- Maintainer  :  R.Murray.1@warwick.ac.uk
-- Stability   :  experimental
--
-- Module describing test cases for expressions and function sensitivity
-- inference.
--
-----------------------------------------------------------------------------

module HFuzz.Types.ExprSpec (main, spec) where

import GHC.TypeLits
import qualified Data.TypeNums as TN
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.HUnit
import Test.QuickCheck
import Test.ShouldNotTypecheck (shouldNotTypecheck)
import HFuzz.Test.Common
import HFuzz.Internal.Types.Expr
import HFuzz.Internal.Types.Context
import HFuzz.Internal.Types.Sens
import HFuzz.Internal.Types.Ty
import Data.Proxy

test1 :: forall t. Expr '[] '[] (TLolli (TPrim t) (SConst (RatOfInt (TN.Pos 2))) (TLolli (TPrim t) (SConst (RatOfInt (TN.Pos 1))) (TPrim (PrimTens t t))))
test1 = XAbs (Var @"x") (XAbs (Var @"y") (XPairT (XVar (Var @"x")) (XPlusCurried (XVar (Var @"x")) (XVar (Var @"y")))))

test2 :: forall proxy x n t1 t2. (KnownSymbol x) => proxy x -> Expr '[V x (ST (SIntConst n) t1)] '[V x (ST (SIntConst ((TN.+) n (TN.Pos 1))) t1)] (TPrim t2)
test2 px = case symbolVal px of
    x -> XVar (Var @x)

test3 :: Expr '[V "x" (ST (SConst (RatOfInt (TN.Pos 0))) (Prim BInt))] '[V "x" (ST (SConst (RatOfInt (TN.Pos 42))) (Prim BInt))] (TPrim (Prim BInt))
test3 = XApp (XAbs (Var @"expr") (XScaleInt (Proxy @(TN.Pos 42)) (XVar (Var @"expr")))) (XVar (Var @"x" @(Prim BInt)))

test4 :: Expr '[V "x" (ST (SConst (RatOfInt (TN.Pos 0))) (Prim BInt))] '[V "x" (ST (SConst (RatOfInt (TN.Pos 30))) (Prim BInt))] (TPrim (Prim BInt))
test4 = XApp (XAbs (Var @"expr1") (XScaleInt (Proxy @(TN.Pos 2)) (XVar (Var @"expr1")))) (XApp (XAbs (Var @"expr2") (XScaleInt (Proxy @(TN.Pos 3)) (XVar (Var @"expr2")))) (XApp (XAbs (Var @"expr3") (XScaleInt (Proxy @(TN.Pos 5)) (XVar (Var @"expr3")))) (XVar (Var @"x" @(Prim BInt)))))

test5 :: forall n. Expr '[] _ (TLolli (TPrim (PrimList n (Prim BNum))) (SConst (TN.Pos 2 TN.:% 1)) (TPrim (PrimList n (Prim (BBool Nothing)))))
test5 = XAbs (Var @"x") (XMap (XAbs (Var @"expr") (XScaleNum (Proxy @(TN.Pos 2 TN.:% 1)) (XVar (Var @"expr")))) (XVar (Var @"x")))

test6 :: forall n. Expr '[] _ (TLolli (TPrim (PrimList n (Prim BNum))) (SConst (RatOfInt (TN.Pos 1))) (TPrim (Prim BNum)))
test6 = XAbs (Var @"x") (XFoldl (XAbs (Var @"expr") (XPlus (XVar (Var @"expr")))) (XCNum (Proxy @(RatOfInt (TN.Pos 0)))) (XVar (Var @"x")))

test7 :: forall t. Expr '[] '[] (TLolli (TPrim t) (SConst (RatOfInt (TN.Pos 2))) (TPrim (PrimTens t t)))
test7 = XAbs (Var @"x") (XLet (Var @"a") (Var @"b") (XVar (Var @"x")) (XPairT (XVar (Var @"a")) (XPlusCurried (XVar (Var @"a")) (XVar (Var @"b")))))

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "Expr" $ do
    describe "XAbs" $ do
        it "captures variable sensitivity" $
            shouldTypecheck $ test1
    describe "XVar" $ do
        it "increments occurrences of the variable" $
            shouldTypecheck $ test2 $ Proxy @"x"
    describe "XApp" $ do
        it "scales output context" $
            shouldTypecheck $ test3
        it "works when multiple applications are composed" $
            shouldTypecheck $ test4
    describe "XMap" $ do
        it "can express doubling the elements of a list" $
            shouldTypecheck $ test5
    describe "XFoldl" $ do
        it "can express the sum of a list" $
            shouldTypecheck $ test6
    describe "XLet" $ do
        it "succesfully eliminates pair types by pattern matching" $
            shouldTypecheck $ test7