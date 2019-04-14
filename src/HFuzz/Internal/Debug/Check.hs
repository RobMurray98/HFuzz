-----------------------------------------------------------------------------
-- |
-- Module      :  HFuzz.Internal.Debug.Check
-- Description :  Debugging methods for inspecting inferred sensitivities
-- Copyright   :  (c) Robert Murray
-- License     :  MIT
--
-- Maintainer  :  R.Murray.1@warwick.ac.uk
-- Stability   :  experimental
--
-- Module providing debug helper functions for inspecting inferred
-- sensitivities and contexts through specifying input contexts to
-- expressions.
--
-----------------------------------------------------------------------------

module HFuzz.Internal.Debug.Check (check, checkWithContext, checkType, checkTypeWithContext) where

import Data.Proxy
import HFuzz.Internal.Types.Expr

-- check the output context of some well-typed expression given an empty input context
check :: Expr '[] ys t -> Proxy ys
check _ = Proxy

-- check the output context of some well-typed expression given a specified input context
checkWithContext :: Proxy xs -> Expr xs ys t -> Proxy ys
checkWithContext _ _ = Proxy

-- check the type of some well-typed expression given an empty input context
checkType :: Expr '[] ys t -> Proxy t
checkType _ = Proxy

-- check the type of some well-typed expression given a specified input context
checkTypeWithContext :: Proxy xs -> Expr xs ys t -> Proxy t
checkTypeWithContext _ _ = Proxy

-- Examples of a sum function and its application
-- :t checkType (abstract (Var @"z" @(PrimList _ (Prim BInt))) (lfoldl add (int (Proxy @(Pos 0))) (ref (Var @"z"))))
-- :t checkType (apply (abstract (Var @"z" @(PrimList _ (Prim BInt))) (lfoldl add (int (Proxy @(Pos 0))) (ref (Var @"z")))) (apply2 lcons (int (Proxy @(Pos 3))) (listOfElem (int (Proxy @(Pos 4))))))

-- Examples of expression sensitivity tracking
-- :t checkTypeWithContext (Proxy @('[V "x" (ST (SIntConst (Pos 0)) (Prim BNum)), V "y" (ST (SIntConst (Pos 0)) (Prim BNum))])) (XPairT (XVar (Var @"x")) (XPlus (XVar (Var @"x")) (XVar (Var @"y"))))
-- :t checkTypeWithContext (Proxy @('[V "x" (ST (SIntConst (Pos 2)) (Prim BNum))])) (XApp (XAbs (Var @"expr") (XScaleInt (Proxy @(Pos 4)) (XVar (Var @"expr")))) (XVar (Var @"x")))

-- Enforce a starting context on definitions using deferred type errors
-- :t checkType fulte