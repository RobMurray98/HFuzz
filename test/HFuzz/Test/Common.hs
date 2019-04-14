-----------------------------------------------------------------------------
-- |
-- Module      :  HFuzz.Test.Common
-- Description :  Common functionality for tests
-- Copyright   :  (c) Robert Murray
-- License     :  MIT
--
-- Maintainer  :  R.Murray.1@warwick.ac.uk
-- Stability   :  experimental
--
-- Module containing common functionality for test cases.
--
-----------------------------------------------------------------------------

module HFuzz.Test.Common (
    shouldTypecheck,
    shouldNotTypecheck,
    (:~:)(..),
    type (/~),
    RatOfInt
    ) where

import Test.Hspec
import Test.Hspec.QuickCheck
import Test.HUnit
import Test.QuickCheck
import Test.ShouldNotTypecheck (shouldNotTypecheck)
import Control.DeepSeq
import Control.Exception
import qualified GHC.TypeLits as G
import HFuzz.Internal.Types.Expr
import qualified Data.TypeNums as TN

-- reference: https://github.com/fpclass/lab10/blob/master/test/Spec.hs

data (a :: k1) :~: (b :: k2) where
    Refl :: a :~: a

instance NFData (a :~: b) where
    rnf Refl = ()

instance NFData (Expr xs ys t) where
    rnf _ = ()

-- `shouldTypecheck` @term@ is an `Assertion` which will succeed if @term@
-- successfully typechecks, i.e. does not throw a runtime type error (due to
-- deferred type errors).
shouldTypecheck :: NFData a => a -> Assertion
shouldTypecheck a = do
    result <- try (evaluate (force a))
    case result of
        Right _              -> return ()
        Left (TypeError msg) -> assertFailure $ "Expected expression to compile but it did not compile: \n" ++ msg
        -- Left (G.TypeError (G.Text msg)) -> assertFailure $ "Expected expression to compile but it did not compile: \n" ++ msg

type family TyEQ a b where
    TyEQ a a = True
    TyEQ a b = False

type a /~ b = TyEQ a b ~ False

type RatOfInt (a :: TN.TInt) = a TN.:% 1