{-# LANGUAGE PartialTypeSignatures #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  HFuzz.Internal.Evaluation.Laplace
-- Description :  Evaluation of expressions, applying the Laplace mechanism
-- Copyright   :  (c) Robert Murray
-- License     :  MIT
--
-- Maintainer  :  R.Murray.1@warwick.ac.uk
-- Stability   :  experimental
--
-- Module providing evaluation methods for interpreting expressions while
-- providing a differential privacy guarantee through application of the
-- Laplace mechanism.
--
-----------------------------------------------------------------------------

module HFuzz.Internal.Evaluation.Laplace (safeEval1, safeEval2, safeEval3, addNoise) where

import GHC.TypeLits
import GHC.Prim(RealWorld)
import Data.TypeNums
import HFuzz.Internal.Evaluation.Evaluation
import HFuzz.Internal.Types.Ty
import HFuzz.Internal.Types.Expr
import HFuzz.Internal.Types.Sens
import HFuzz.Internal.Data.Value
import System.Random.MWC.Probability
import System.Random.MWC
import Data.Proxy
import Control.Applicative
import Control.Monad
import Control.Monad.Primitive
import Control.Monad.IO.Class
import Control.Monad.Trans.Class

-- differentially-private wrapper function for evalExpr1. The last argument is the chosen privacy cost epsilon.
safeEval1 :: (ToUExpr a, KnownSens s) => Expr '[] ys (TLolli (TPrim _) s (TPrim _)) -> a -> Double -> IO Value
safeEval1 e x eps = addNoise l createSystemRandom (evalExpr1 e x)
    where
        l = laplace 0.0 ((fromRational $ reifySens $ getSens1 e) / eps)

-- differentially-private wrapper function for evalExpr2. The last argument is the chosen privacy cost epsilon.
safeEval2 :: (ToUExpr a, ToUExpr b, KnownSens s1, KnownSens s2) => Expr '[] ys (TLolli (TPrim _) s1 (TLolli (TPrim _) s2 (TPrim _))) -> a -> b -> Double -> IO Value
safeEval2 e a b eps = addNoise l createSystemRandom (evalExpr2 e a b)
        where
            l = laplace 0.0 ((fromRational $ reifySens $ getSens2 e) / eps)

-- differentially-private wrapper function for evalExpr3. The last argument is the chosen privacy cost epsilon.
safeEval3 :: (ToUExpr a, ToUExpr b, ToUExpr c, KnownSens s1, KnownSens s2, KnownSens s3) => Expr '[] ys (TLolli (TPrim _) s1 (TLolli (TPrim _) s2 (TLolli (TPrim _) s3 (TPrim _)))) -> a -> b -> c -> Double -> IO Value
safeEval3 e a b c eps = addNoise l createSystemRandom (evalExpr3 e a b c)
        where
            l = laplace 0.0 ((fromRational $ reifySens $ getSens3 e) / eps)

-- apply additive noise to a Value according to some distribution and some pseudo-random number generator
addNoise :: Prob IO Double -> IO (Gen RealWorld) -> Value -> IO Value
addNoise l r (VInt i) = do
    n <- r >>= sample l
    return $ VNum $ (fromIntegral i) + n
addNoise l r (VNum i) = do
    n <- r >>= sample l
    return $ VNum $ i + n
addNoise l r (VBool b) = fail "cannot apply Laplacian noise to boolean value"
addNoise l r (VString s) = fail "cannot apply Laplacian noise to string value"
addNoise l r (VList xs) = do
    ys <- mapM (addNoise l r) xs
    return $ VList ys
addNoise l r (VPair x y) = do
    a <- addNoise l r x
    b <- addNoise l r y
    return $ VPair a b
addNoise l r VUnit = fail "cannot apply Laplacian noise to unit value"

-- PRIVATE:

-- getSens1, getSens2 and getSens3 are helper functions to extract function sensitivity annotations

getSens1 :: Expr '[] ys (TLolli (TPrim _) s (TPrim _)) -> Proxy s
getSens1 e = Proxy

getSens2 :: Expr '[] ys (TLolli (TPrim _) s1 (TLolli (TPrim _) s2 (TPrim _))) -> Proxy (SMult s1 s2)
getSens2 e = Proxy

getSens3 :: Expr '[] ys (TLolli (TPrim _) s1 (TLolli (TPrim _) s2 (TLolli (TPrim _) s3 (TPrim _)))) -> Proxy (SMult s3 (SMult s1 s2))
getSens3 e = Proxy