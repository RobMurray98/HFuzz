{-# LANGUAGE PartialTypeSignatures #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  HFuzz.Safe.Evaluation
-- Description :  Differentially private functions for evaluating expressions
-- Copyright   :  (c) Robert Murray
-- License     :  MIT
--
-- Maintainer  :  R.Murray.1@warwick.ac.uk
-- Stability   :  experimental
--
-- Module acting as an interface to HFuzz.Internal.Evaluation.Laplace,
-- renaming and exporting differentially private wrapper functions which
-- apply the Laplace mechanism to evaluation methods.
--
-----------------------------------------------------------------------------


module HFuzz.Safe.Evaluation (run0, run1, run2, run3, addNoise) where

import HFuzz.Internal.Evaluation.Laplace
import HFuzz.Internal.Evaluation.Evaluation
import HFuzz.Internal.Types.Ty
import HFuzz.Internal.Types.Expr
import HFuzz.Internal.Types.Sens
import HFuzz.Internal.Data.Value

-- evaluate a closed expression, returning a constant value
run0 :: Expr '[] ys t -> Value
run0 = evalExpr
-- evaluate a function expression applied to a value with some chosen privacy cost epsilon
run1 :: (ToUExpr a, KnownSens s) => Expr '[] ys (TLolli (TPrim _) s (TPrim _)) -> a -> Double -> IO Value
run1 = safeEval1
-- evaluate a function expression taking two arguments applied to two values with some chosen privacy cost epsilon
run2 :: (ToUExpr a, ToUExpr b, KnownSens s1, KnownSens s2) => Expr '[] ys (TLolli (TPrim _) s1 (TLolli (TPrim _) s2 (TPrim _))) -> a -> b -> Double -> IO Value
run2 = safeEval2
-- evaluate a function expression taking three arguments applied to three values with some chosen privacy cost epsilon
run3 :: (ToUExpr a, ToUExpr b, ToUExpr c, KnownSens s1, KnownSens s2, KnownSens s3) => Expr '[] ys (TLolli (TPrim _) s1 (TLolli (TPrim _) s2 (TLolli (TPrim _) s3 (TPrim _)))) -> a -> b -> c -> Double -> IO Value
run3 = safeEval3