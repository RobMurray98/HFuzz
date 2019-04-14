{-# LANGUAGE PartialTypeSignatures #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  HFuzz.Unsafe.Evaluation
-- Description :  Unsafe functions for evaluating expressions
-- Copyright   :  (c) Robert Murray
-- License     :  MIT
--
-- Maintainer  :  R.Murray.1@warwick.ac.uk
-- Stability   :  experimental
--
-- Module acting as an interface to HFuzz.Internal.Evaluation.Evaluation,
-- renaming and exporting evaluation functions which do not apply the
-- Laplace mechanism. Primarily for testing purposes to ensure correct
-- operation of expressions.
--
-----------------------------------------------------------------------------

module HFuzz.Unsafe.Evaluation (run0, run1, run2, run3) where

import HFuzz.Internal.Evaluation.Evaluation
import HFuzz.Internal.Types.Ty
import HFuzz.Internal.Types.Expr
import HFuzz.Internal.Types.Sens
import HFuzz.Internal.Data.Value

-- evaluate a closed expression, returning a constant value
run0 :: Expr '[] ys t -> Value
run0 = evalExpr
-- evaluate a function expression applied to a value
run1 :: (ToUExpr a) => Expr '[] ys (TLolli (TPrim _) s (TPrim _)) -> a -> Value
run1 = evalExpr1
-- evaluate a function expression taking two arguments applied to two values
run2 :: (ToUExpr a, ToUExpr b) => Expr '[] ys (TLolli (TPrim _) s1 (TLolli (TPrim _) s2 (TPrim _))) -> a -> b -> Value
run2 = evalExpr2
-- evaluate a function expression taking three arguments applied to three values
run3 :: (ToUExpr a, ToUExpr b, ToUExpr c) => Expr '[] ys (TLolli (TPrim _) s1 (TLolli (TPrim _) s2 (TLolli (TPrim _) s3 (TPrim _)))) -> a -> b -> c -> Value
run3 = evalExpr3