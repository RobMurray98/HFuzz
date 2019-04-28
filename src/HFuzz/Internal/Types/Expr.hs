{-# LANGUAGE UndecidableInstances #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  HFuzz.Internal.Types.Expr
-- Description :  Expressions for function sensitivity inference
-- Copyright   :  (c) Robert Murray
-- License     :  MIT
--
-- Maintainer  :  R.Murray.1@warwick.ac.uk
-- Stability   :  experimental
--
-- Module providing the Expr data type for the expression of statements
-- which undergo function sensitivity inference through the use of
-- type parameter input and output contexts tracking the number of uses
-- of variables.
--
-----------------------------------------------------------------------------

module HFuzz.Internal.Types.Expr (Expr(..), Var(..)) where

import HFuzz.Internal.Types.Ty
import HFuzz.Internal.Types.Sens
import HFuzz.Internal.Types.Context

import Data.Maybe
import GHC.TypeLits
import qualified Data.TypeNums as TN
import Data.TypeNums(KnownInt(..), KnownRat(..))
import Data.Proxy
import Data.Type.Bool
import HFuzz.Internal.Data.Bool
import Data.Data

data Var (s :: Symbol) (t :: PrimTy) where
    Var :: (KnownSymbol s) => Var s t

data Expr (xs :: Context) (ys :: Context) (t :: Ty) where

    -- variables, abstraction and application
    XVar :: (KnownSymbol x, UseFromCtx x xs ~ ys) => Var x pt -> Expr xs ys (TPrim pt)
    XAbs :: (KnownSymbol x, GetSens x as bs ~ s, AddToCtx x (ST (SConst (TN.Pos 0 TN.:% 1)) t1) xs ~ as, RemoveFromCtx x bs ~ ds) => Var x t1 -> Expr as bs t2 -> Expr xs ds (TLolli (TPrim t1) s t2)
    XAbsInf :: (KnownSymbol x, AddToCtx x (ST SInf t1) xs ~ as, RemoveFromCtx x bs ~ ds) => Var x t1 -> Expr as bs t2 -> Expr xs ds (TLolli (TPrim t1) SInf t2)
    XApp :: (ScaleCtx xs zs s ~ as) => Expr xs ys (TLolli t1 s t2) -> Expr ys zs t1 -> Expr xs as t2
    
    -- pair types
    XPairT :: Expr xs ys (TPrim pt1) -> Expr ys zs (TPrim pt2) -> Expr xs zs (TPrim (PrimTens pt1 pt2))
    XPairA :: (CtxMatch xs1 ys1 xs2 ys2 ~ '(as, bs)) => Expr xs1 ys1 (TPrim pt1) -> Expr xs2 ys2 (TPrim pt2) -> Expr as bs (TPrim (PrimAmp pt1 pt2)) -- contexts are weakened and combined
    
    -- constant values
    XCNum :: (KnownRat r) => Proxy r -> Expr xs xs (TPrim (Prim BNum))
    XCInt :: (KnownInt i) => Proxy i -> Expr xs xs (TPrim (Prim BInt))
    XCBool :: (KnownBool b) => Proxy b -> Expr xs xs (TPrim (Prim (BBool (Just b))))
    XCTrue :: Expr xs xs (TPrim (Prim (BBool (Just True))))
    XCFalse :: Expr xs xs (TPrim (Prim (BBool (Just False))))
    XCEmptyList :: SPrimTy pt -> Expr xs xs (TPrim (PrimList 0 pt))
    XCListOfElem :: Expr xs ys (TPrim pt) -> Expr xs ys (TPrim (PrimList 1 pt))
    XCString :: (KnownSymbol s) => Proxy s -> Expr xs xs (TPrim (Prim BString))
    XCUnit :: Expr xs xs (TPrim (Prim BUnit))
    
    -- numerical operations
    XPlusCurried :: Expr xs ys (TPrim pt) -> Expr ys zs (TPrim pt) -> Expr xs zs (TPrim pt)
    XMinusCurried :: Expr xs ys (TPrim pt) -> Expr ys zs (TPrim pt) -> Expr xs zs (TPrim pt)
    XTimesCurried :: (ScaleCtx xs zs SInf ~ as) => Expr xs ys (TPrim pt) -> Expr ys zs (TPrim pt) -> Expr xs as (TPrim pt)
    XPlus :: Expr xs ys (TPrim (PrimTens pt pt)) -> Expr xs ys (TPrim pt)
    XMinus :: Expr xs ys (TPrim (PrimTens pt pt)) -> Expr xs ys (TPrim pt)
    XTimes :: (ScaleCtx xs ys SInf ~ as) => Expr xs ys (TPrim (PrimTens pt pt)) -> Expr xs as (TPrim pt)
    XScaleNum :: (KnownRat r, ScaleCtx ys zs (SConst r) ~ as) => Proxy r -> Expr ys zs t -> Expr ys as t
    XScaleInt :: (KnownInt i, ScaleCtx ys zs (SIntConst i) ~ as) => Proxy i -> Expr ys zs t -> Expr ys as t

    -- pair operations
    XSwapT :: Expr xs ys (TPrim (PrimTens t1 t2)) -> Expr xs ys (TPrim (PrimTens t2 t1))
    XSwapA :: Expr xs ys (TPrim (PrimAmp t1 t2)) -> Expr xs ys (TPrim (PrimAmp t2 t1))
    XLet   :: (KnownSymbol x, KnownSymbol y,
               AddToCtx x (ST (SConst (TN.Pos 0 TN.:% 1)) pt1) xs ~ zs,
               AddToCtx y (ST (SConst (TN.Pos 0 TN.:% 1)) pt2) zs ~ as,
               GetSens x as bs ~ s1, GetSens y as bs ~ s2,
               s ~ SLub s1 s2,
               RemoveFromCtx x bs ~ cs, RemoveFromCtx y cs ~ ds,
               ScaleCtx xs ys s ~ es,
               CtxAdd ds es ~ fs) => Var x pt1 -> Var y pt2 -> Expr xs ys (TPrim (PrimTens pt1 pt2)) -> Expr as bs t -> Expr xs fs t
    XFstA  :: Expr xs ys (TPrim (PrimAmp t1 t2)) -> Expr xs ys (TPrim t1)
    XSndA  :: Expr xs ys (TPrim (PrimAmp t1 t2)) -> Expr xs ys (TPrim t2)
    
    -- list operations
    XCons :: Expr xs ys (TPrim pt) -> Expr ys zs (TPrim (PrimList n pt)) -> Expr xs zs (TPrim (PrimList (n + 1) pt))
    XHead :: ((TN.>=) n 1) => Expr xs ys (TPrim (PrimList n pt)) -> Expr xs ys (TPrim (PrimTens pt (PrimList (n - 1) pt)))
    XFoldl :: (ScaleCtx ys zs s ~ as) => Expr xs xs (TLolli (TPrim (PrimTens pt1 pt2)) s (TPrim pt1)) -> Expr xs ys (TPrim pt1) -> Expr ys zs (TPrim (PrimList n pt2)) -> Expr xs as (TPrim pt1)
    XFoldr :: (ScaleCtx ys zs s ~ as) => Expr xs xs (TLolli (TPrim (PrimTens pt1 pt2)) s (TPrim pt2)) -> Expr xs ys (TPrim pt2) -> Expr ys zs (TPrim (PrimList n pt1)) -> Expr xs as (TPrim pt2)
    XFoldl1 :: ((TN.>=) n 1, ScaleCtx ys zs s ~ as) => Expr ys ys (TLolli (TPrim (PrimTens pt pt)) s (TPrim pt)) -> Expr ys zs (TPrim (PrimList n pt)) -> Expr ys as (TPrim pt)
    XFoldr1 :: ((TN.>=) n 1, ScaleCtx ys zs s ~ as) => Expr ys ys (TLolli (TPrim (PrimTens pt pt)) s (TPrim pt)) -> Expr ys zs (TPrim (PrimList n pt)) -> Expr ys as (TPrim pt)
    XMap :: (ScaleCtx xs ys s ~ zs) => Expr xs xs (TLolli (TPrim pt1) s (TPrim pt2)) -> Expr xs ys (TPrim (PrimList n pt1)) -> Expr xs zs (TPrim (PrimList n pt2))

    -- conditional operations
    XIfElse :: (CombineIfElse mb zs as ~ bs) => Expr xs ys (TPrim (Prim (BBool mb))) -> Expr xs zs t -> Expr xs as t -> Expr xs bs t
    
    -- type-casting
    XCastIntBool :: (ScaleCtx xs ys SInf ~ as) => Expr xs ys (TPrim (Prim BInt)) -> Expr xs as (TPrim (Prim (BBool Nothing)))
    XCastNumBool :: (ScaleCtx xs ys SInf ~ as) => Expr xs ys (TPrim (Prim BNum)) -> Expr xs as (TPrim (Prim (BBool Nothing)))
    XCastIntNum  :: Expr xs ys (TPrim (Prim BInt)) -> Expr xs ys (TPrim (Prim BNum))
    XCastBoolInt :: Expr xs ys (TPrim (Prim (BBool b))) -> Expr xs ys (TPrim (Prim BInt))
    XCastBoolNum :: Expr xs ys (TPrim (Prim (BBool b))) -> Expr xs ys (TPrim (Prim BNum))
    XCastNumInt  :: (ScaleCtx xs ys SInf ~ as) => Expr xs ys (TPrim (Prim BNum)) -> Expr xs as (TPrim (Prim BInt))

    -- comparisons
    XEQ :: (ScaleCtx xs zs SInf ~ as) => Expr xs ys (TPrim pt) -> Expr ys zs (TPrim pt) -> Expr xs as (TPrim (Prim (BBool Nothing)))
    XGT :: (ScaleCtx xs zs SInf ~ as) => Expr xs ys (TPrim pt) -> Expr ys zs (TPrim pt) -> Expr xs as (TPrim (Prim (BBool Nothing)))
    XLT :: (ScaleCtx xs zs SInf ~ as) => Expr xs ys (TPrim pt) -> Expr ys zs (TPrim pt) -> Expr xs as (TPrim (Prim (BBool Nothing)))
    XGTE :: (ScaleCtx xs zs SInf ~ as) => Expr xs ys (TPrim pt) -> Expr ys zs (TPrim pt) -> Expr xs as (TPrim (Prim (BBool Nothing)))
    XLTE :: (ScaleCtx xs zs SInf ~ as) => Expr xs ys (TPrim pt) -> Expr ys zs (TPrim pt) -> Expr xs as (TPrim (Prim (BBool Nothing)))

    -- boolean operations
    XAnd :: (ScaleCtx xs zs SInf ~ as) => Expr xs ys (TPrim (Prim (BBool x))) -> Expr ys zs (TPrim (Prim (BBool y))) -> Expr xs as (TPrim (Prim (BBool Nothing)))
    XOr  :: (ScaleCtx xs zs SInf ~ as) => Expr xs ys (TPrim (Prim (BBool x))) -> Expr ys zs (TPrim (Prim (BBool y))) -> Expr xs as (TPrim (Prim (BBool Nothing)))
    XNot :: (ScaleCtx xs ys SInf ~ as) => Expr xs ys (TPrim (Prim (BBool x))) -> Expr xs as (TPrim (Prim (BBool Nothing)))

instance Show (Expr xs ys t) where
    show (XVar v) = "XVar " ++ (symbolVal $ varToString v)
    show (XAbs v e) = "XAbs " ++ (symbolVal $ varToString v) ++ " (" ++ show e ++ ")"
    show (XAbsInf v e) = "XAbsInf " ++ (symbolVal $ varToString v) ++ " (" ++ show e ++ ")"
    show (XApp f e) = "XApp (" ++ show f ++ ") (" ++ show e ++ ")"
    show (XPairT a b) = "XPairT (" ++ show a ++ ") (" ++ show b ++ ")"
    show (XPairA a b) = "XPairA (" ++ show a ++ ") (" ++ show b ++ ")"
    show (XCNum pr) = "XCNum " ++ show (TN.ratVal pr)
    show (XCInt pi) = "XCInt " ++ show (TN.intVal pi)
    show (XCBool pb) = "XCBool " ++ show (boolVal pb)
    show XCTrue = "XCTrue"
    show XCFalse = "XCFalse"
    show (XCEmptyList _) = "XCEmptyList"
    show (XCListOfElem x) = "XCListOfElem (" ++ show x ++ ")"
    show (XCString ps) = "XCString " ++ symbolVal ps
    show XCUnit = "XCUnit"
    show (XPlusCurried a b) = "XPlusCurried (" ++ show a ++ ") (" ++ show b ++ ")"
    show (XPlus x) = "XPlus (" ++ show x ++ ")"
    show (XMinusCurried a b) = "XMinusCurried (" ++ show a ++ ") (" ++ show b ++ ")"
    show (XMinus x) = "XMinus (" ++ show x ++ ")"
    show (XScaleNum pr x) = "XScaleNum " ++ show (TN.ratVal pr) ++ " (" ++ show x ++ ")"
    show (XScaleInt pi x) = "XScaleInt " ++ show (TN.intVal pi) ++ " (" ++ show x ++ ")"
    show (XTimesCurried a b) = "XTimes (" ++ show a ++ ") (" ++ show b ++ ")"
    show (XTimes x) = "XTimes (" ++ show x ++ ")"
    show (XSwapT x) = "XSwapT (" ++ show x ++ ")"
    show (XSwapA x) = "XSwapA (" ++ show x ++ ")"
    show (XLet x y e1 e2) = "XLet (" ++ (symbolVal $ varToString x) ++ ", " ++ (symbolVal $ varToString y) ++ ") = (" ++ show e1 ++ ") in (" ++ show e2 ++ ")"
    show (XFstA x) = "XFstA (" ++ show x ++ ")"
    show (XSndA x) = "XSndA (" ++ show x ++ ")"
    show (XCons x xs) = "XCons (" ++ show x ++ ") (" ++ show xs ++ ")"
    show (XHead xs) = "XHead (" ++ show xs ++ ")"
    show (XFoldl f n xs) = "XFoldl (" ++ show f ++ ") (" ++ show n ++ ") (" ++ show xs ++ ")"
    show (XFoldr f n xs) = "XFoldr (" ++ show f ++ ") (" ++ show n ++ ") (" ++ show xs ++ ")"
    show (XFoldl1 f xs) = "XFoldl1 (" ++ show f ++ ") (" ++ show xs ++ ")"
    show (XFoldr1 f xs) = "XFoldr1 (" ++ show f ++ ") (" ++ show xs ++ ")"
    show (XMap f xs) = "XMap (" ++ show f ++ ") (" ++ show xs ++ ")"
    show (XIfElse b x y) = "XIfElse (" ++ show b ++ ") (" ++ show x ++ ") (" ++ show y ++ ")"
    show (XCastIntBool x) = "XCastIntBool (" ++ show x ++ ")"
    show (XCastNumBool x) = "XCastNumBool (" ++ show x ++ ")"
    show (XCastIntNum x) = "XCastIntNum (" ++ show x ++ ")"
    show (XCastBoolInt x) = "XCastBoolInt (" ++ show x ++ ")"
    show (XCastBoolNum x) = "XCastBoolNum (" ++ show x ++ ")"
    show (XCastNumInt x) = "XCastNumInt (" ++ show x ++ ")"
    show (XEQ x y) = "XEQ (" ++ show x ++ ") (" ++ show y ++ ")"
    show (XGT x y) = "XGT (" ++ show x ++ ") (" ++ show y ++ ")"
    show (XLT x y) = "XLT (" ++ show x ++ ") (" ++ show y ++ ")"
    show (XGTE x y) = "XGTE (" ++ show x ++ ") (" ++ show y ++ ")"
    show (XLTE x y) = "XLTE (" ++ show x ++ ") (" ++ show y ++ ")"
    show (XAnd x y) = "XAnd (" ++ show x ++ ") (" ++ show y ++ ")"
    show (XOr x y) = "XOr (" ++ show x ++ ") (" ++ show y ++ ")"
    show (XNot x) = "XNot (" ++ show x ++ ")"

-- PRIVATE:

type family AND (x :: Maybe Bool) (y :: Maybe Bool) :: Maybe Bool where
    AND Nothing _ = Nothing
    AND _ Nothing = Nothing
    AND (Just x) (Just y) = Just (x && y)

type family OR (x :: Maybe Bool) (y :: Maybe Bool) :: Maybe Bool where
    OR Nothing _ = Nothing
    OR _ Nothing = Nothing
    OR (Just x) (Just y) = Just (x || y)

type family NOT (x :: Maybe Bool) :: Maybe Bool where
    NOT Nothing = Nothing
    Not (Just x) = Just (Not x)

type family CombineIfElse (b :: Maybe Bool) (as :: Context) (bs :: Context) where
    CombineIfElse Nothing as bs = CtxLub as bs
    CombineIfElse (Just True) as _ = as
    CombineIfElse (Just False) _ bs = bs

varToString :: (KnownSymbol s) => Var s t -> Proxy s
varToString _ = Proxy