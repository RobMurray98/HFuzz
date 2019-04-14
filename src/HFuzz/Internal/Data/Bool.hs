-----------------------------------------------------------------------------
-- |
-- Module      :  HFuzz.Internal.Data.Bool
-- Description :  Reification of type-level booleans
-- Copyright   :  (c) Robert Murray
-- License     :  MIT
--
-- Maintainer  :  R.Murray.1@warwick.ac.uk
-- Stability   :  experimental
--
-- Module providing the type class KnownBool and method boolVal for the
-- reification of type-level booleans from GHC.TypeLits.
--
-----------------------------------------------------------------------------

module HFuzz.Internal.Data.Bool (SBool(..), KnownBool, boolVal) where

import GHC.TypeLits
import Data.Type.Bool
import Data.Proxy

newtype SBool (b :: Bool) = SBool Bool

class KnownBool b where
    boolSing :: SBool b

instance KnownBool True where
    boolSing = SBool True

instance KnownBool False where
    boolSing = SBool False

boolVal :: forall proxy b. KnownBool b => proxy b -> Bool
boolVal _ = case boolSing :: SBool b of
    SBool b -> b