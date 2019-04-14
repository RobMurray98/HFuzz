{-# OPTIONS_GHC -fdefer-type-errors #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  HFuzz.Types.SensSpec
-- Description :  Test cases for sensitivities
-- Copyright   :  (c) Robert Murray
-- License     :  MIT
--
-- Maintainer  :  R.Murray.1@warwick.ac.uk
-- Stability   :  experimental
--
-- Module describing test cases for sensitivities.
--
-----------------------------------------------------------------------------

module HFuzz.Types.SensSpec (main, spec) where

import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck
import HFuzz.Internal.Types.Sens
import HFuzz.Test.Common
import qualified Data.TypeNums as TN

-- SimplifySens must be called multiple times as it has a limited depth to ensure that it terminates
test1 :: SimplifySens (SAdd (SimplifySens (SLub (SimplifySens (SMult (SDiff (SIntConst (TN.Pos 4)) (SIntConst (TN.Neg 2))) (SIntConst (TN.Pos 6)))) (SIntConst (TN.Pos 42)))) (SimplifySens (SGlb (SInf) (SLub (SNegInf) (SIntConst (TN.Neg 3)))))) :~: SIntConst (TN.Pos 39)
test1 = Refl

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "Sens" $ do
    describe "SimplifySens" $ do
      it "simplifies a complex sensitivity statement" $
        shouldTypecheck $ test1