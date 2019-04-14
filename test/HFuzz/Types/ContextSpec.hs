{-# OPTIONS_GHC -fdefer-type-errors #-}
{-# LANGUAGE PartialTypeSignatures, AllowAmbiguousTypes #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  HFuzz.Types.ContextSpec
-- Description :  Test cases for contexts
-- Copyright   :  (c) Robert Murray
-- License     :  MIT
--
-- Maintainer  :  R.Murray.1@warwick.ac.uk
-- Stability   :  experimental
--
-- Module describing test cases for contexts.
--
-----------------------------------------------------------------------------

module HFuzz.Types.ContextSpec (main, spec) where

import GHC.TypeLits
import qualified Data.TypeNums as TN
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.HUnit
import Test.QuickCheck
import Test.ShouldNotTypecheck (shouldNotTypecheck)
import HFuzz.Internal.Types.Context
import HFuzz.Test.Common
import HFuzz.Internal.Types.Sens
import HFuzz.Internal.Types.Ty
import Data.Ord
import Data.Type.Bool
import Data.Proxy

main :: IO ()
main = hspec spec

test1 :: forall x s st. (x ~ s) => Lookup s '[V x st] :~: Just '(x, st)
test1 = Refl

test2 :: forall st. Lookup "s" '[V "x" st] :~: (Nothing :: Maybe (Symbol, SensTy))
test2 = Refl

test3 :: forall x st. AddToCtx x st '[] :~: '[V x st]
test3 = Refl

test4 :: forall x y z s1 t1 s2 t2. (x ~ y) => UseFromCtx x '[V y (ST (SIntConst s1) t1), V z (ST s2 t2)] :~: '[V y (ST (SIntConst ((TN.+) s1 (TN.Pos 1))) t1), V z (ST s2 t2)]
test4 = Refl

test5 :: forall x t. (CtxMatch '[V x (ST (SIntConst (TN.Pos 2)) t)] '[V x (ST (SIntConst (TN.Pos 5)) t)] '[V x (ST (SIntConst (TN.Pos 3)) t)] '[V x (ST (SIntConst (TN.Pos 7)) t)]) :~: '( '[V x (ST (SIntConst (TN.Pos 5)) t)], '[V x (ST (SIntConst (TN.Pos 9)) t)])
test5 = Refl

test6 :: forall x t a b c d. (CtxCombine '[V x (ST (SIntConst a) t)] '[V x (ST (SIntConst b) t)] '[V x (ST (SIntConst c) t)] '[V x (ST (SIntConst d) t)]) :~: '( '[V x (ST (SIntConst ((TN.+) a c)) t)], '[V x (ST (SIntConst ((TN.+) b d)) t)])
test6 = Refl

test7 :: forall t a b. (CtxAdd '[V "x" (ST SInf (Prim BUnit)), V "y" (ST (SIntConst a) t)] '[V "y" (ST (SIntConst b) t), V "z" (ST SInf (Prim BUnit))]) :~: '[V "x" (ST SInf (Prim BUnit)), V "y" (ST (SIntConst ((TN.+) a b)) t), V "z" (ST SInf (Prim BUnit))]
test7 = Refl

test8 :: forall a b c d t1 t2 n. (ScaleCtx '[V "x" (ST (SIntConst a) t1), V "y" (ST (SIntConst b) t2)] '[V "x" (ST (SIntConst c) t1), V "y" (ST (SIntConst d) t2)] (SIntConst n)) :~: '[V "x" (ST (SIntConst ((TN.+) a ((TN.*) ((TN.-) c a) n))) t1), V "y" (ST (SIntConst ((TN.+) b ((TN.*) ((TN.-) d b) n))) t2)]
test8 = Refl

test9 :: forall x a b t. (GetSens x '[V x (ST (SIntConst a) t)] '[V x (ST (SIntConst b) t)]) :~: SIntConst ((TN.-) b a)
test9 = Refl

test10 :: forall x st. (GetSens x '[V x st] '[]) :~: SIntConst (TN.Pos 0)
test10 = Refl

test11 :: (GetAndRemoveFromCtx "y" '[V "x" (ST SInf (Prim BUnit)), V "y" (ST SInf (Prim BUnit))]) :~: '(Just '("y", (ST SInf (Prim BUnit))), '[V "x" (ST SInf (Prim BUnit))])
test11 = Refl

test12 :: forall st1 st2. (GetAndRemoveFromCtx "z" '[V "x" st1, V "y" st2]) :~: ( '(Nothing, '[V "x" st1, V "y" st2]) :: (Maybe (Symbol, SensTy), Context))
test12 = Refl

spec :: Spec
spec = do
  describe "Context" $ do
    describe "Lookup" $ do
      it "finds a present variable" $
        shouldTypecheck $ test1
      it "returns nothing when variable not present" $
        shouldTypecheck $ test2
    describe "AddToCtx" $ do
      it "adds the variable to an empty context" $
        shouldTypecheck $ test3
    describe "UseFromCtx" $ do
      it "increments usage of variable" $ 
        shouldTypecheck $ test4
    describe "CtxMatch" $ do
      it "weakens and combines contexts" $
        shouldTypecheck $ test5
    describe "CtxCombine" $ do
      it "adds contexts" $
        shouldTypecheck $ test6
    describe "CtxAdd" $ do
      it "performs context addition" $
        shouldTypecheck $ test7
    describe "ScaleCtx" $ do
      it "scales variable usages" $
        shouldTypecheck $ test8
    describe "GetSens" $ do
      it "gets the sensitivity of a variable" $
        shouldTypecheck $ test9
      it "returns zero when not present in the output context" $
        shouldTypecheck $ test10
    describe "GetAndRemoveFromCtx" $ do
      it "gets and removes a present variable" $
        shouldTypecheck $ test11
      it "leaves the context unchanged when variable not present" $
        shouldTypecheck $ test12