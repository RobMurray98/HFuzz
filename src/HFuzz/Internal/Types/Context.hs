{-# LANGUAGE UndecidableInstances #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  HFuzz.Internal.Types.Context
-- Description :  Type-level contexts for expressions
-- Copyright   :  (c) Robert Murray
-- License     :  MIT
--
-- Maintainer  :  R.Murray.1@warwick.ac.uk
-- Stability   :  experimental
--
-- Module providing type-level contexts and various type families for
-- operating on them.
--
-----------------------------------------------------------------------------

module HFuzz.Internal.Types.Context (
    CVar(..),
    Context(..),
    SensTy(..),
    EmptyContext,
    Lookup,
    ExtendCtx,
    AddToCtx,
    UseFromCtx,
    RemoveFromCtx,
    CtxCombine,
    CtxMatch,
    CtxAdd,
    ScaleCtx,
    GetSens,
    GetAndRemoveFromCtx) where

import HFuzz.Internal.Types.Sens
import HFuzz.Internal.Types.Ty
import Data.TypeNums(TInt(..), Nat(..))
import qualified Data.TypeNums as TN
import Data.Maybe(Maybe(..))
import Data.Type.Bool
import GHC.TypeLits

-- data type to hold some type-level string and some arbitrary type a
data CVar a where
    V :: Symbol -> a -> CVar a

-- type representing a list of variables and their related a
type Ctx a = [CVar a]

-- data type to hold a sensitivity and a primitive type
data SensTy where
    ST :: Sens -> PrimTy -> SensTy

-- type to represent a context
type Context = Ctx SensTy

type family EmptyContext :: Context where
    EmptyContext = '[]

-- lookup of a variable in a context
type family Lookup (id :: Symbol) (c :: Context) :: Maybe (Symbol, SensTy) where
    Lookup id '[] = 'Nothing
    Lookup id ((V id st) ': xs) = Just '(id, st)
    Lookup id (_ ': xs) = Lookup id xs

-- add a variable to the head of a context
type family ExtendCtx (id :: Symbol) (st :: SensTy) (c :: Context) :: Context where
    ExtendCtx id st c = V id st ': c

-- add a variable to the head of a context if it is not already present in the context
type family AddToCtx (x :: Symbol) (st :: SensTy) (xs :: Context) :: Context where
    AddToCtx x st xs = AddToCtx' (Lookup x xs) x st xs

-- increment uses of a variable in a context
type family UseFromCtx (x :: Symbol) (xs :: Context) :: Context where
    UseFromCtx x '[] = TypeError (Text "Variable undefined")
    UseFromCtx x ((V x (ST s t)) ': xs) = V x (ST (SimplifySens (SAdd s (SIntConst (TN.Pos 1)))) t) ': xs
    UseFromCtx x (y ': xs) = y ': UseFromCtx x xs

-- find and remove a variable from a context if it is present
type family RemoveFromCtx (x :: Symbol) (xs :: Context) :: Context where
    RemoveFromCtx x '[] = '[]
    RemoveFromCtx x ((V x (ST s t)) ': xs) = xs
    RemoveFromCtx x (y ': xs) = y ': RemoveFromCtx x xs

-- weaken two input/output context pairs respective to each other, for &-pair introduction
type family CtxMatch (xs1 :: Context) (ys1 :: Context) (xs2 :: Context) (ys2 :: Context) :: (Context, Context) where
    CtxMatch xs1 ys1 xs2 ys2 = '((CtxAdd xs1 xs2), (CtxAdd (CtxAdd xs1 xs2) (CtxLub (CtxDiff xs1 ys1) (CtxDiff xs2 ys2))))

-- combine two input/output context pairs
type family CtxCombine (xs1 :: Context) (ys1 :: Context) (xs2 :: Context) (ys2 :: Context) :: (Context, Context) where
    CtxCombine xs1 ys1 xs2 ys2 = '((CtxAdd xs1 xs2), (CtxAdd ys1 ys2))
    
-- add two contexts
type family CtxAdd (as :: Context) (bs :: Context) :: Context where
    CtxAdd '[] bs = bs
    -- CtxAdd as ((V b (ST s1 t)) ': bs) = CtxAdd' (GetAndRemoveFromCtx b as) (V b (ST s1 t)) bs
    CtxAdd ((V a (ST s1 t)) ': as) bs = CtxAdd' (GetAndRemoveFromCtx a bs) (V a (ST s1 t)) as

-- scale an input/output context pair by some sensitivity value
type family ScaleCtx (xs :: Context) (ys :: Context) (s :: Sens) :: Context where
    ScaleCtx xs '[] _ = xs
    ScaleCtx xs ((V y (ST s1 t)) ': ys) s2 = ScaleCtx' (GetAndRemoveFromCtx y xs) (V y (ST s1 t)) s2 ys

-- get the sensitivity of a variable between an input and output context
type family GetSens (x :: Symbol) (as :: Context) (bs :: Context) :: Sens where
    GetSens x as bs = GetSens' (Lookup x as) (Lookup x bs)

-- remove and return a variable from a context if it is present
type family GetAndRemoveFromCtx (x :: Symbol) (c :: Context) :: (Maybe (Symbol, SensTy), Context) where
    GetAndRemoveFromCtx x xs = GetAndRemoveFromCtx' x xs '[]

-- PRIVATE:

type family CtxLub (as :: Context) (bs :: Context) :: Context where
    CtxLub _ '[] = '[]
    CtxLub as ((V b (ST s1 t)) ': bs) = CtxLub' (GetAndRemoveFromCtx b as) (V b (ST s1 t)) bs

type family CtxDiff (as :: Context) (bs :: Context) :: Context where
    CtxDiff _ '[] = '[]
    CtxDiff as ((V b (ST s1 t)) ': bs) = CtxDiff' (GetAndRemoveFromCtx b as) (V b (ST s1 t)) bs

type family ScaleCtx' (r :: (Maybe (Symbol, SensTy), Context)) (x :: CVar SensTy) (s :: Sens) (ys :: Context) :: Context where
    ScaleCtx'  '(Nothing, _) _ _ _ = TypeError (Text "Variable in output context not present in input context")
    ScaleCtx'  '(Just '(x, (ST s1 t)), xs) (V x (ST s2 t)) s3 ys = (V x (ST (SimplifySens (SAdd s1 (SMult (SDiff s2 s1) s3))) t)) ': ScaleCtx xs ys s3

type family AddToCtx' (m :: Maybe (Symbol, SensTy)) (x :: Symbol) (st :: SensTy) (xs :: Context) :: Context where
    AddToCtx' Nothing x st xs = ExtendCtx x st xs
    AddToCtx' (Just _) _ _ _  = TypeError (Text "Already in Context")

type family CtxAdd' (r :: (Maybe (Symbol, SensTy), Context)) (x :: CVar SensTy) (xs :: Context) :: Context where
    CtxAdd'  '(Nothing, zs) x xs = x ': CtxAdd xs zs
    CtxAdd'  '(Just '(b, (ST s1 t)), bs) (V b (ST s2 t)) as = (V b (ST (SimplifySens (SAdd s2 s1)) t)) ': CtxAdd as bs

type family CtxLub' (r :: (Maybe (Symbol, SensTy), Context)) (x :: CVar SensTy) (xs :: Context) :: Context where
    CtxLub'  '(Nothing, _) _ _ = TypeError (Text "Variable in output context not present in input context")
    CtxLub'  '(Just '(x, (ST s1 t)), zs) (V x (ST s2 t)) xs = (V x (ST (SimplifySens (SLub s1 s2)) t)) ': CtxLub xs zs

type family CtxDiff' (r :: (Maybe (Symbol, SensTy), Context)) (x :: CVar SensTy) (xs :: Context) :: Context where
    CtxDiff'  '(Nothing, _) _ _ = TypeError (Text "Variable in output context not present in input context")
    CtxDiff'  '(Just '(x, (ST s1 t)), zs) (V x (ST s2 t)) xs = (V x (ST (SimplifySens (SDiff s2 s1)) t)) ': CtxDiff xs zs

type family GetSens' (a :: Maybe (Symbol, SensTy)) (b :: Maybe (Symbol, SensTy)) :: Sens where
    GetSens' _ Nothing = SIntConst (TN.Pos 0)
    GetSens' Nothing (Just x) = TypeError (Text "Variable in output context not present in input context")
    GetSens' (Just '(x, (ST s1 t))) (Just '(x, (ST s2 t))) = SimplifySens (SDiff s2 s1)

type family GetAndRemoveFromCtx' (x :: Symbol) (c :: Context) (z :: Context) :: (Maybe (Symbol, SensTy), Context) where
    GetAndRemoveFromCtx' _ '[] zs = '(Nothing, Reverse zs)
    GetAndRemoveFromCtx' x ((V x (ST s t)) ': xs) z = '(Just '(x, (ST s t)), ConcatLists z xs)
    GetAndRemoveFromCtx' x (a ': xs) z = GetAndRemoveFromCtx' x xs (a ': z)

type family ConcatLists (xs :: [k]) (ys :: [k]) :: [k] where
    ConcatLists xs '[] = xs
    ConcatLists '[] ys = ys
    ConcatLists (x ': xs) ys = x ': (ConcatLists xs ys)

type family Reverse (xs :: [k]) :: [k] where
    Reverse xs = Reverse' xs '[]

type family Reverse' (xs :: [k]) (ys :: [k]) :: [k] where
    Reverse' (x ': xs) ys = Reverse' xs (x ': ys)
    Reverse'  '[] ys = ys
    