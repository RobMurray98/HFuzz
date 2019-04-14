-----------------------------------------------------------------------------
-- |
-- Module      :  HFuzz.Evaluation.EvaluationSpec
-- Description :  Test cases for evaluation
-- Copyright   :  (c) Robert Murray
-- License     :  MIT
--
-- Maintainer  :  R.Murray.1@warwick.ac.uk
-- Stability   :  experimental
--
-- Module describing test cases for evaluation.
--
-----------------------------------------------------------------------------

module HFuzz.Evaluation.EvaluationSpec (main, spec) where

import Test.Hspec
import Test.Hspec.QuickCheck
import Test.HUnit
import Test.QuickCheck
import HFuzz.Primitives.Math
import HFuzz.Internal.Evaluation.Evaluation
import HFuzz.Internal.Data.Value

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
    describe "Evaluation" $ do
        describe "evalExpr1" $ do
            prop "correctly evalautes the sum of a list" $ \(x :: [Double]) ->
                evalExpr1 xsum x `shouldBe` (VNum $ sum x)
        describe "evalExpr2" $ do
            prop "correctly evaluates the product of two values" $ \(x :: Double) -> \(y :: Double) ->
                evalExpr2 fcurriedTimes x y `shouldBe` (VNum $ x * y)