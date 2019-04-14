{-# LANGUAGE UndecidableInstances, MultiParamTypeClasses #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  HFuzz.Internal.Types.Sens
-- Description :  Sensitivity values
-- Copyright   :  (c) Robert Murray
-- License     :  MIT
--
-- Maintainer  :  R.Murray.1@warwick.ac.uk
-- Stability   :  experimental
--
-- Module providing data types for sensitivity values for use in both
-- annotating functions with their sensitivity, and for describing the number
-- of uses of variables in contexts.
--
-----------------------------------------------------------------------------

module HFuzz.Internal.Types.Sens (
    Sens(..),
    KnownSens(..),
    SimplifySens,
    EvalSens
    ) where

import qualified Data.TypeNums as TN
import GHC.TypeLits
import Data.TypeNums(Rat(..), Nat(..), TInt(..), KnownRat(..), KnownInt(..), ratVal, intVal)
import Data.Type.Bool
import Data.Proxy
import Data.Lub (lub, HasLub(..), flatLub)
import Data.Glb (glb, HasGlb(..), flatGlb)
import Data.Ratio

-- data type for sensitivity values
data Sens where
    SInf      :: Sens -- positive infinity
    SNegInf   :: Sens -- negative infinity
    SConst    :: Rat -> Sens -- constant rational sensitivity value
    SIntConst :: TInt -> Sens -- constant integer sensitivity value
    SAdd      :: Sens -> Sens -> Sens -- addition of two sensitivity values
    SMult     :: Sens -> Sens -> Sens -- multiplication of two sensitivity values
    SLub      :: Sens -> Sens -> Sens -- least upper bound of two sensitivity values
    SGlb      :: Sens -> Sens -> Sens -- greatest lower bound of two sensitivity values
    SDiff     :: Sens -> Sens -> Sens -- the difference between two sensitivities values

instance HasLub Rational where lub = flatLub
instance HasGlb Rational where glb = flatGlb

-- class that allows reification of type-level sensitivities
class KnownSens (n :: Sens) where
    reifySens :: Proxy n -> Rational 

instance KnownSens SInf where
    reifySens p = 1 % 0

instance KnownSens SNegInf where
    reifySens p = -1 % 0

instance KnownRat r => KnownSens (SConst r) where
    reifySens p = ratVal (Proxy @r)

instance KnownInt i => KnownSens (SIntConst i) where
    reifySens p = intVal (Proxy @i) % 1

instance (KnownSens x, KnownSens y) => KnownSens (SAdd x y) where
    reifySens p = (reifySens (Proxy @x)) + (reifySens (Proxy @y))

instance (KnownSens x, KnownSens y) => KnownSens (SMult x y) where
    reifySens p = (reifySens (Proxy @x)) * (reifySens (Proxy @y))

instance (KnownSens x, KnownSens y) => KnownSens (SDiff x y) where
    reifySens p = (reifySens (Proxy @x)) - (reifySens (Proxy @y))

instance (KnownSens x, KnownSens y) => KnownSens (SLub x y) where
    reifySens p = (reifySens (Proxy @x)) `lub` (reifySens (Proxy @y))

instance (KnownSens x, KnownSens y) => KnownSens (SGlb x y) where
    reifySens p = (reifySens (Proxy @x)) `glb` (reifySens (Proxy @y))

-- type family to simplify a sensitivity value
type family SimplifySens (s :: Sens) :: Sens where
    SimplifySens SInf = SInf
    SimplifySens SNegInf = SNegInf
    SimplifySens (SConst r) = SConst r
    SimplifySens (SIntConst i) = SIntConst i
    SimplifySens (SAdd _ SInf) = SInf
    SimplifySens (SAdd SInf _) = SInf
    SimplifySens (SAdd _ SNegInf) = SNegInf
    SimplifySens (SAdd SNegInf _) = SNegInf
    SimplifySens (SAdd (SConst r1) (SConst r2)) = SConst ((TN.+) r1 r2)
    SimplifySens (SAdd (SConst r) (SIntConst i)) = SConst ((TN.+) r i)
    SimplifySens (SAdd (SIntConst i) (SConst r)) = SConst ((TN.+) i r)
    SimplifySens (SAdd (SIntConst i1) (SIntConst i2)) = SIntConst ((TN.+) i1 i2)
    SimplifySens (SAdd s1 s2) = SimplifySens' (SAdd (SimplifySens s1) (SimplifySens s2))
    SimplifySens (SMult _ SInf) = SInf
    SimplifySens (SMult SInf _) = SInf
    SimplifySens (SMult _ SNegInf) = SNegInf
    SimplifySens (SMult SNegInf _) = SNegInf
    SimplifySens (SMult (SConst r1) (SConst r2)) = SConst ((TN.*) r1 r2)
    SimplifySens (SMult (SConst r) (SIntConst i)) = SConst ((TN.*) r i)
    SimplifySens (SMult (SIntConst i) (SConst r)) = SConst ((TN.*) i r)
    SimplifySens (SMult (SIntConst i1) (SIntConst i2)) = SIntConst ((TN.*) i1 i2)
    SimplifySens (SMult s1 s2) = SimplifySens' (SMult (SimplifySens s1) (SimplifySens s2))
    SimplifySens (SLub _ SInf) = SInf
    SimplifySens (SLub SInf _) = SInf
    SimplifySens (SLub x SNegInf) = x
    SimplifySens (SLub SNegInf x) = x
    SimplifySens (SLub (SConst r1) (SConst r2)) = SConst (Lub r1 r2)
    SimplifySens (SLub (SConst r) (SIntConst i)) = SConst (LubRI r i)
    SimplifySens (SLub (SIntConst i) (SConst r)) = SConst (LubIR i r)
    SimplifySens (SLub (SIntConst i1) (SIntConst i2)) = SIntConst (Lub i1 i2)
    SimplifySens (SLub s1 s2) = SimplifySens' (SLub (SimplifySens s1) (SimplifySens s2))
    SimplifySens (SGlb x SInf) = x
    SimplifySens (SGlb SInf x) = x
    SimplifySens (SGlb _ SNegInf) = SNegInf
    SimplifySens (SGlb SNegInf _) = SNegInf
    SimplifySens (SGlb (SConst r1) (SConst r2)) = SConst (Glb r1 r2)
    SimplifySens (SGlb (SConst r) (SIntConst i)) = SConst (GlbRI r i)
    SimplifySens (SGlb (SIntConst i) (SConst r)) = SConst (GlbIR i r)
    SimplifySens (SGlb (SIntConst i1) (SIntConst i2)) = SIntConst (Glb i1 i2)
    SimplifySens (SGlb s1 s2) = SimplifySens' (SGlb (SimplifySens s1) (SimplifySens s2))
    SimplifySens (SDiff _ SInf) = SNegInf
    SimplifySens (SDiff SInf _) = SInf
    SimplifySens (SDiff _ SNegInf) = SInf
    SimplifySens (SDiff SNegInf _) = SNegInf
    SimplifySens (SDiff (SConst r1) (SConst r2)) = SConst ((TN.-) r1 r2)
    SimplifySens (SDiff (SConst r) (SIntConst i)) = SConst ((TN.-) r i)
    SimplifySens (SDiff (SIntConst i) (SConst r)) = SConst ((TN.-) i r)
    SimplifySens (SDiff (SIntConst i1) (SIntConst i2)) = SIntConst ((TN.-) i1 i2)
    SimplifySens (SDiff s1 s2) = SimplifySens' (SDiff (SimplifySens s1) (SimplifySens s2))
    SimplifySens s = s

-- type family to evaluate a type-level sensitivity to a type-level rational
type family EvalSens (s :: Sens) :: Rat where
    EvalSens SInf = 1 :% 0
    EvalSens SNegInf = (TN.Neg 1) :% 0
    EvalSens (SConst r) = r
    EvalSens (SIntConst i) = i :% 1
    EvalSens (SAdd s1 s2) = (TN.+) (EvalSens s1) (EvalSens s2)
    EvalSens (SMult s1 s2) = (TN.*) (EvalSens s1) (EvalSens s2)
    EvalSens (SLub s1 s2) = Lub (EvalSens s1) (EvalSens s2)
    EvalSens (SGlb s1 s2) = Glb (EvalSens s1) (EvalSens s2)
    EvalSens (SDiff s1 s2) = (TN.-) (EvalSens s1) (EvalSens s2)
    
-- PRIVATE:

type family Lub (x :: k) (y :: k) :: k where
    Lub x y = If ((TN.<=?) x y) y x

type family Glb (x :: k) (y :: k) :: k where
    Glb x y = If ((TN.<=?) x y) x y

type family LubRI (x :: Rat) (y :: TInt) :: Rat where
    LubRI x y = If ((TN.<=?) x y) ((TN.:%) y 1) x

type family LubIR (x :: TInt) (y :: Rat) :: Rat where
    LubIR x y = If ((TN.<=?) x y) y ((TN.:%) x 1)

type family GlbRI (x :: Rat) (y :: TInt) :: Rat where
    GlbRI x y = If ((TN.<=?) x y) x ((TN.:%) y 1)

type family GlbIR (x :: TInt) (y :: Rat) :: Rat where
    GlbIR x y = If ((TN.<=?) x y) ((TN.:%) x 1) y

type family SimplifySens' (s :: Sens) :: Sens where
    SimplifySens' SInf = SInf
    SimplifySens' SNegInf = SNegInf
    SimplifySens' (SConst r) = SConst r
    SimplifySens' (SIntConst i) = SIntConst i
    SimplifySens' (SAdd _ SInf) = SInf
    SimplifySens' (SAdd SInf _) = SInf
    SimplifySens' (SAdd _ SNegInf) = SNegInf
    SimplifySens' (SAdd SNegInf _) = SNegInf
    SimplifySens' (SAdd (SConst r1) (SConst r2)) = SConst ((TN.+) r1 r2)
    SimplifySens' (SAdd (SConst r) (SIntConst i)) = SConst ((TN.+) r i)
    SimplifySens' (SAdd (SIntConst i) (SConst r)) = SConst ((TN.+) i r)
    SimplifySens' (SAdd (SIntConst i1) (SIntConst i2)) = SIntConst ((TN.+) i1 i2)
    SimplifySens' (SAdd s1 s2) = SAdd s1 s2
    SimplifySens' (SMult _ SInf) = SInf
    SimplifySens' (SMult SInf _) = SInf
    SimplifySens' (SMult _ SNegInf) = SNegInf
    SimplifySens' (SMult SNegInf _) = SNegInf
    SimplifySens' (SMult (SConst r1) (SConst r2)) = SConst ((TN.*) r1 r2)
    SimplifySens' (SMult (SConst r) (SIntConst i)) = SConst ((TN.*) r i)
    SimplifySens' (SMult (SIntConst i) (SConst r)) = SConst ((TN.*) i r)
    SimplifySens' (SMult (SIntConst i1) (SIntConst i2)) = SIntConst ((TN.*) i1 i2)
    SimplifySens' (SMult s1 s2) = SMult s1 s2
    SimplifySens' (SLub _ SInf) = SInf
    SimplifySens' (SLub SInf _) = SInf
    SimplifySens' (SLub x SNegInf) = x
    SimplifySens' (SLub SNegInf x) = x
    SimplifySens' (SLub (SConst r1) (SConst r2)) = SConst (Lub r1 r2)
    SimplifySens' (SLub (SConst r) (SIntConst i)) = SConst (LubRI r i)
    SimplifySens' (SLub (SIntConst i) (SConst r)) = SConst (LubIR i r)
    SimplifySens' (SLub (SIntConst i1) (SIntConst i2)) = SIntConst (Lub i1 i2)
    SimplifySens' (SLub s1 s2) = SLub s1 s2
    SimplifySens' (SGlb x SInf) = x
    SimplifySens' (SGlb SInf x) = x
    SimplifySens' (SGlb _ SNegInf) = SNegInf
    SimplifySens' (SGlb SNegInf _) = SNegInf
    SimplifySens' (SGlb (SConst r1) (SConst r2)) = SConst (Glb r1 r2)
    SimplifySens' (SGlb (SConst r) (SIntConst i)) = SConst (GlbRI r i)
    SimplifySens' (SGlb (SIntConst i) (SConst r)) = SConst (GlbIR i r)
    SimplifySens' (SGlb (SIntConst i1) (SIntConst i2)) = SIntConst (Glb i1 i2)
    SimplifySens' (SGlb s1 s2) = SGlb s1 s2
    SimplifySens' (SDiff _ SInf) = SNegInf
    SimplifySens' (SDiff SInf _) = SInf
    SimplifySens' (SDiff _ SNegInf) = SInf
    SimplifySens' (SDiff SNegInf _) = SNegInf
    SimplifySens' (SDiff (SConst r1) (SConst r2)) = SConst ((TN.-) r1 r2)
    SimplifySens' (SDiff (SConst r) (SIntConst i)) = SConst ((TN.-) r i)
    SimplifySens' (SDiff (SIntConst i) (SConst r)) = SConst ((TN.-) i r)
    SimplifySens' (SDiff (SIntConst i1) (SIntConst i2)) = SIntConst ((TN.-) i1 i2)
    SimplifySens' (SDiff s1 s2) = (SDiff s1 s2)
    SimplifySens' s = s