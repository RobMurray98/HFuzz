{-# LANGUAGE UndecidableInstances, PartialTypeSignatures #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  HFuzz.Internal.Evaluation.Evaluation
-- Description :  Evaluation of expressions without Laplacian noise
-- Copyright   :  (c) Robert Murray
-- License     :  MIT
--
-- Maintainer  :  R.Murray.1@warwick.ac.uk
-- Stability   :  experimental
--
-- Module providing evaluation methods for interpreting expressions. Methods
-- are available for evaluating a closed expression and the application of
-- functions to arguments.
--
-----------------------------------------------------------------------------

module HFuzz.Internal.Evaluation.Evaluation (
    evalExpr,
    evalExpr1,
    evalExpr2,
    evalExpr3,
    UExpr(..),
    ToUExpr,
    untype
    ) where

import Prelude hiding (lookup)
import HFuzz.Internal.Types.Expr
import HFuzz.Internal.Data.Value
import Data.TypeNums
import GHC.TypeLits
import Data.Proxy
import HFuzz.Internal.Data.Bool
import HFuzz.Internal.Types.Ty
import Data.Map.Strict

-- evaluate a closed expression
evalExpr :: Expr '[] ys t -> Value
evalExpr e = evaluate' empty (untype e)

-- evaluate the application of a function to a value
evalExpr1 :: (ToUExpr a) => Expr '[] ys (TLolli (TPrim _) s (TPrim _)) -> a -> Value
evalExpr1 e x = evaluate' empty $ UXApp (untype e) (exprOf x)

-- evaluate the application of a function taking two arguments to two values
evalExpr2 :: (ToUExpr a, ToUExpr b) => Expr '[] ys (TLolli (TPrim _) s1 (TLolli (TPrim _) s2 (TPrim _))) -> a -> b -> Value
evalExpr2 e x y = case untype e of 
    UXAbs a (UXAbs b e') -> evaluate' empty $ UXApp (UXAbs a (UXApp (UXAbs b e') (exprOf y))) (exprOf x)

-- evaluate the application of a function taking three arguments to three values
evalExpr3 :: (ToUExpr a, ToUExpr b, ToUExpr c) => Expr '[] ys (TLolli (TPrim _) s1 (TLolli (TPrim _) s2 (TLolli (TPrim _) s3 (TPrim _)))) -> a -> b -> c -> Value
evalExpr3 e x y z = case untype e of 
    UXAbs a (UXAbs b (UXAbs c e')) -> evaluate' empty $ UXApp (UXAbs a (UXApp (UXAbs b (UXApp (UXAbs c e') (exprOf z))) (exprOf y))) (exprOf z)

-- mirror type for Expr without type constraints and with type-level constants reified
data UExpr where
    -- variables, abstraction and application
    UXVar :: String -> UExpr
    UXAbs :: String -> UExpr -> UExpr
    UXApp :: UExpr -> UExpr -> UExpr
    
    -- pair types
    UXPair :: UExpr -> UExpr -> UExpr
    
    -- constant values
    UXCNum :: Double -> UExpr
    UXCInt :: Int -> UExpr
    UXCBool :: Bool -> UExpr
    UXCEmptyList :: UExpr
    UXCString :: String -> UExpr
    UXCUnit :: UExpr
    
    -- numerical operations
    UXPlusCurried :: UExpr -> UExpr -> UExpr
    UXPlus :: UExpr -> UExpr
    UXMinusCurried :: UExpr -> UExpr -> UExpr
    UXMinus :: UExpr -> UExpr
    UXScaleNum :: Double -> UExpr -> UExpr
    UXScaleInt :: Int -> UExpr -> UExpr
    UXTimesCurried :: UExpr -> UExpr -> UExpr
    UXTimes :: UExpr -> UExpr

    -- pair operations
    UXSwap :: UExpr -> UExpr
    UXLet :: String -> String -> UExpr -> UExpr -> UExpr
    UXFst :: UExpr -> UExpr
    UXSnd :: UExpr -> UExpr
    
    -- list operations
    UXCons :: UExpr -> UExpr -> UExpr
    UXHead :: UExpr -> UExpr
    UXFoldl :: UExpr -> UExpr -> UExpr -> UExpr
    UXFoldr :: UExpr -> UExpr -> UExpr -> UExpr
    UXFoldl1 :: UExpr -> UExpr -> UExpr
    UXFoldr1 :: UExpr -> UExpr -> UExpr
    UXMap :: UExpr -> UExpr -> UExpr

    -- conditional operations
    UXIfElse :: UExpr -> UExpr -> UExpr -> UExpr
    
    -- type-casting
    UXCastIntBool :: UExpr -> UExpr
    UXCastNumBool :: UExpr -> UExpr
    UXCastIntNum  :: UExpr -> UExpr
    UXCastBoolInt :: UExpr -> UExpr
    UXCastBoolNum :: UExpr -> UExpr
    UXCastNumInt  :: UExpr -> UExpr

    -- comparisons
    UXEQ :: UExpr -> UExpr -> UExpr
    UXGT :: UExpr -> UExpr -> UExpr
    UXLT :: UExpr -> UExpr -> UExpr
    UXGTE :: UExpr -> UExpr -> UExpr
    UXLTE :: UExpr -> UExpr -> UExpr

    -- boolean operations
    UXAnd :: UExpr -> UExpr -> UExpr
    UXOr  :: UExpr -> UExpr -> UExpr
    UXNot :: UExpr -> UExpr
    deriving Show

-- create an UExpr mirror expression of an Expr such that it can be evaluated
untype :: Expr xs ys t -> UExpr

-- variables, abstraction and application
untype (XVar v) = UXVar $ symbolVal $ varToString v
untype (XAbs v e) = UXAbs (symbolVal $ varToString v) (untype e)
untype (XAbsInf v e) = UXAbs (symbolVal $ varToString v) (untype e)
untype (XApp f e) = UXApp (untype f) (untype e)

-- pair types
untype (XPairT e1 e2) = UXPair (untype e1) (untype e2)
untype (XPairA e1 e2) = UXPair (untype e1) (untype e2)

-- constant values
untype (XCNum pr) = UXCNum $ fromRational $ ratVal pr
untype (XCInt pi) = UXCInt $ fromInteger $ intVal pi
untype (XCBool pb) = UXCBool $ boolVal pb
untype XCTrue = UXCBool True
untype XCFalse = UXCBool False
untype (XCEmptyList _) = UXCEmptyList
untype (XCListOfElem e) = UXCons (untype e) UXCEmptyList
untype (XCString ps) = UXCString $ symbolVal ps
untype XCUnit = UXCUnit

-- numerical operations
untype (XPlusCurried e1 e2) = UXPlusCurried (untype e1) (untype e2)
untype (XPlus e) = UXPlus $ untype e
untype (XMinusCurried e1 e2) = UXMinusCurried (untype e1) (untype e2)
untype (XMinus e) = UXMinus $ untype e
untype (XScaleNum pr e) = UXScaleNum (fromRational $ ratVal pr) (untype e)
untype (XScaleInt pi e) = UXScaleInt (fromIntegral $ intVal pi) (untype e)
untype (XTimesCurried e1 e2) = UXTimesCurried (untype e1) (untype e2)
untype (XTimes e) = UXTimes $ untype e

-- pair operations
untype (XSwapT e) = UXSwap $ untype e
untype (XSwapA e) = UXSwap $ untype e
untype (XLet x y e1 e2) = UXLet (symbolVal $ varToString x) (symbolVal $ varToString y) (untype e1) (untype e2)
untype (XFstA e) = UXFst $ untype e
untype (XSndA e) = UXSnd $ untype e

-- list operations
untype (XCons e1 e2) = UXCons (untype e1) (untype e2)
untype (XHead e) = UXHead $ untype e
untype (XFoldl f n e) = UXFoldl (untype f) (untype n) (untype e)
untype (XFoldr f n e) = UXFoldr (untype f) (untype n) (untype e)
untype (XFoldl1 f e) = UXFoldl1 (untype f) (untype e)
untype (XFoldr1 f e) = UXFoldr1 (untype f) (untype e)
untype (XMap f e) = UXMap (untype f) (untype e)

-- conditional operations
untype (XIfElse b e1 e2) = UXIfElse (untype b) (untype e1) (untype e2)

-- type-casting
untype (XCastIntBool e) = UXCastIntBool $ untype e
untype (XCastNumBool e) = UXCastNumBool $ untype e
untype (XCastIntNum e) = UXCastIntNum $ untype e
untype (XCastBoolInt e) = UXCastBoolInt $ untype e
untype (XCastBoolNum e) = UXCastBoolNum $ untype e
untype (XCastNumInt e) = UXCastNumInt $ untype e

-- comparisons
untype (XEQ e1 e2) = UXEQ (untype e1) (untype e2)
untype (XGT e1 e2) = UXGT (untype e1) (untype e2)
untype (XLT e1 e2) = UXLT (untype e1) (untype e2)
untype (XGTE e1 e2) = UXGTE (untype e1) (untype e2)
untype (XLTE e1 e2) = UXLTE (untype e1) (untype e2)

-- boolean operations
untype (XAnd e1 e2) = UXAnd (untype e1) (untype e2)
untype (XOr e1 e2) = UXOr (untype e1) (untype e2)
untype (XNot e) = UXNot $ untype e

-- class describing types that can be used as arguments for evaluation
class ToUExpr a where
    exprOf :: a -> UExpr

instance (Real a) => ToUExpr a where
    exprOf a = UXCNum $ fromRational $ toRational a

instance {-# OVERLAPPING #-} (ToUExpr a) => ToUExpr [a] where
    exprOf (x:xs) = UXCons (exprOf x) (exprOf xs)
    exprOf [] = UXCEmptyList

instance ToUExpr Bool where
    exprOf a = UXCBool a

instance ToUExpr Unit where
    exprOf u = UXCUnit

instance ToUExpr String where
    exprOf s = UXCString s

instance {-# OVERLAPPING #-} (ToUExpr a, ToUExpr b) => ToUExpr (a, b) where
    exprOf (x, y) = UXPair (exprOf x) (exprOf y) 

-- PRIVATE:

type RuntimeContext = Map String Value

-- convert a Value to a constant constructor of UExpr such that it can be used in evaluation
valueToUExpr :: Value -> UExpr
valueToUExpr v = case v of
    VBool b -> UXCBool b
    VInt i -> UXCInt i
    VNum n -> UXCNum n
    VString s -> UXCString s
    VList (x:xs) -> UXCons (valueToUExpr x) (valueToUExpr $ VList xs)
    VList [] -> UXCEmptyList
    VPair x y -> UXPair (valueToUExpr x) (valueToUExpr y)
    VUnit -> UXCUnit

-- helper function for building a left-fold
formFoldl :: UExpr -> UExpr -> UExpr -> UExpr -> UExpr
formFoldl f n x (UXCons y ys) = case UXApp f (UXPair n x) of
    n' -> formFoldl f n' y ys
formFoldl f n x UXCEmptyList = UXApp f (UXPair n x)
formFoldl f n x _ = error "foldl applied to non-list type"

-- helper function for building a right-fold
formFoldr :: UExpr -> UExpr -> UExpr -> UExpr -> UExpr
formFoldr f n x (UXCons y ys) = case formFoldr f n y ys of
    y' -> UXApp f (UXPair x y')
formFoldr f n x UXCEmptyList = UXApp f (UXPair x n)
formFoldr f n x _ = error "foldr applied to non-list type"

-- helper function for building a map
formMap :: UExpr -> UExpr -> UExpr
formMap f UXCEmptyList = UXCEmptyList
formMap f (UXCons x xs) = UXCons (UXApp f x) (formMap f xs)

-- helper function to extract name of variable from a Var type such that it can be reified by symbolVal
varToString :: (KnownSymbol s) => Var s t -> Proxy s
varToString _ = Proxy

-- evaluation logic of module
evaluate' :: RuntimeContext -> UExpr -> Value

-- variables, abstraction and application
evaluate' c (UXVar v) = case lookup v c of
    Just v -> v
    Nothing -> error $ "variable used not available in context: " ++ v
evaluate' c (UXAbs v e) = error $ "cannot evaluate unapplied function: " ++ show (UXAbs v e)
evaluate' c (UXApp (UXAbs x e1) e2) = evaluate' (insert x (evaluate' c e2) c) e1
evaluate' c (UXApp f e) = error $ "cannot apply non-function expression: " ++ show f

-- pair types
evaluate' c (UXPair e1 e2) = VPair (evaluate' c e1) (evaluate' c e2)

-- constant values
evaluate' c (UXCNum n) = VNum n
evaluate' c (UXCInt n) = VInt n
evaluate' c (UXCBool b) = VBool b
evaluate' c UXCEmptyList = VList []
evaluate' c (UXCString s) = VString s
evaluate' c UXCUnit = VUnit

-- numerical operations
evaluate' c (UXPlusCurried e1 e2) = case vplus v1 v2 of
    Just a -> a
    Nothing -> error "addition applied to non-numeric values"
    where
        v1 = evaluate' c e1
        v2 = evaluate' c e2
evaluate' c (UXPlus e) = case evaluate' c e of
    VPair x y -> case vplus x y of
        Just a -> a
        Nothing -> error "addition applied to non-numeric values"
    _ -> error "addition applied to non-pair value"
evaluate' c (UXMinusCurried e1 e2) = case vminus v1 v2 of
    Just a -> a
    Nothing -> error "subtraction applied to non-numeric values"
    where
        v1 = evaluate' c e1
        v2 = evaluate' c e2
evaluate' c (UXMinus e) = case evaluate' c e of
    VPair x y -> case vminus x y of
        Just a -> a
        Nothing -> error "subtraction applied to non-numeric values"
    _ -> error "subtraction applied to non-pair value"
evaluate' c (UXScaleNum r e) = case vtimes (VNum $ r) (evaluate' c e) of
        Just a -> a
        Nothing -> error "multiplication by a constant applied to non-numeric value"
evaluate' c (UXScaleInt i e) = case vtimes (VInt $ i) (evaluate' c e) of
    Just a -> a
    Nothing -> error "multiplication by a constant applied to non-numeric value"
evaluate' c (UXTimesCurried e1 e2) = case vtimes v1 v2 of
    Just a -> a
    Nothing -> error "multiplication applied to non-numeric values"
    where
        v1 = evaluate' c e1
        v2 = evaluate' c e2
evaluate' c (UXTimes e) = case evaluate' c e of
    VPair x y -> case vtimes x y of
        Just a -> a
        Nothing -> error "multiplication applied to non-numeric values"
    _ -> error "multiplication applied to non-pair value"

-- pair operations
evaluate' c (UXSwap e) = case evaluate' c e of
    (VPair x y) -> VPair y x
    _ -> error "swap operation applied to non-pair type"
evaluate' c (UXLet x y e1 e2) = case evaluate' c e1 of
    (VPair a b) -> evaluate' (insert x a (insert y b c)) e2
    _ -> error "let applied to non-pair type"
evaluate' c (UXFst e) = case evaluate' c e of
    (VPair x _) -> x
    _ -> error "fst operation applied to non-pair type"
evaluate' c (UXSnd e) = case evaluate' c e of
    (VPair _ y) -> y
    _ -> error "snd operation applied to non-pair type"

-- list operations
evaluate' c (UXCons e1 e2) = case evaluate' c e2 of
    VList xs -> VList $ (evaluate' c e1) : xs 
    _ -> error "cons operation applied to non-list type"
evaluate' c (UXHead e) = case evaluate' c e of
    VList (x:xs) -> VPair x (VList xs)
    VList [] -> error "head operation applied to empty list"
    _ -> error "head operation applied to non-list type"
evaluate' c (UXFoldl f n (UXCons x xs)) = evaluate' c $ formFoldl f n x xs
evaluate' c (UXFoldl f n UXCEmptyList) = evaluate' c n
evaluate' c (UXFoldl f n (UXVar x)) = evaluate' c $ UXFoldl f n (valueToUExpr $ evaluate' c $ UXVar x)
evaluate' c (UXFoldl f n _) = error "foldl applied to non-list value"
evaluate' c (UXFoldr f n (UXCons x xs)) = evaluate' c $ formFoldr f n x xs
evaluate' c (UXFoldr f n UXCEmptyList) = evaluate' c n
evaluate' c (UXFoldr f n (UXVar x)) = evaluate' c $ UXFoldr f n (valueToUExpr $ evaluate' c $ UXVar x)
evaluate' c (UXFoldr f n _) = error "foldr applied to non-list value"
evaluate' c (UXFoldl1 f (UXCons x UXCEmptyList)) = evaluate' c x
evaluate' c (UXFoldl1 f (UXCons x (UXCons y ys))) = evaluate' c $ formFoldl f x y ys
evaluate' c (UXFoldl1 f (UXVar x)) = evaluate' c $ UXFoldl1 f (valueToUExpr $ evaluate' c $ UXVar x)
evaluate' c (UXFoldl1 f UXCEmptyList) = error "foldl1 applied to empty list"
evaluate' c (UXFoldl1 f _) = error "foldl1 applied to non-list value"
evaluate' c (UXFoldr1 f (UXCons x UXCEmptyList)) = evaluate' c x
evaluate' c (UXFoldr1 f (UXCons x (UXCons y ys))) = evaluate' c $ formFoldl f x y ys
evaluate' c (UXFoldr1 f (UXVar x)) = evaluate' c $ UXFoldr1 f (valueToUExpr $ evaluate' c $ UXVar x)
evaluate' c (UXFoldr1 f UXCEmptyList) = error "foldr1 applied to empty list"
evaluate' c (UXFoldr1 f _) = error "foldr1 applied to non-list value"
evaluate' c (UXMap f (UXCons x xs)) = evaluate' c $ formMap f (UXCons x xs)
evaluate' c (UXMap f UXCEmptyList) = VList []
evaluate' c (UXMap f (UXVar x)) = evaluate' c $ UXMap f (valueToUExpr $ evaluate' c $ UXVar x)
evaluate' c (UXMap f _) = error "map applied to non-list value"

-- conditional operations
evaluate' c (UXIfElse b e1 e2) = case evaluate' c b of
    (VBool True) -> evaluate' c e1
    (VBool False) -> evaluate' c e2
    v -> case castToBool v of
        Just (VBool True) -> evaluate' c e1
        Just (VBool False) -> evaluate' c e2
        Nothing -> error "conditional statement of if else statement could not be reduced to a boolean value"

-- type-casting
evaluate' c (UXCastIntBool e) = case castToBool v of
    Just a -> a
    Nothing -> error "cast int to bool applied to non-numeric value"
    where
        v = evaluate' c e
evaluate' c (UXCastNumBool e) = case castToBool v of
    Just a -> a
    Nothing -> error "cast num to bool applied to non-numeric value"
    where
        v = evaluate' c e
evaluate' c (UXCastIntNum e) = case castToNum v of
    Just a -> a
    Nothing -> error "cast int to num applied to non-numeric value"
    where
        v = evaluate' c e
evaluate' c (UXCastBoolInt e) = case castToInt v of
    Just a -> a
    Nothing -> error "cast bool to int applied to non-numeric value"
    where
        v = evaluate' c e
evaluate' c (UXCastBoolNum e) = case castToNum v of
    Just a -> a
    Nothing -> error "cast bool to num applied to non-numeric value"
    where
        v = evaluate' c e
evaluate' c (UXCastNumInt e) = case castToInt v of
    Just a -> a
    Nothing -> error "cast num to int applied to non-numeric value"
    where
        v = evaluate' c e

-- comparisons
evaluate' c (UXEQ e1 e2) = case veq v1 v2 of
    Just a -> a
    Nothing -> error "equality operator applied to non-numeric values"
    where
        v1 = evaluate' c e1
        v2 = evaluate' c e2
evaluate' c (UXGT e1 e2) = case vgt v1 v2 of
    Just a -> a
    Nothing -> error "> operator applied to non-numeric values"
    where
        v1 = evaluate' c e1
        v2 = evaluate' c e2
evaluate' c (UXLT e1 e2) = case vlt v1 v2 of
    Just a -> a
    Nothing -> error "< operator applied to non-numeric values"
    where
        v1 = evaluate' c e1
        v2 = evaluate' c e2
evaluate' c (UXGTE e1 e2) = case vgte v1 v2 of
    Just a -> a
    Nothing -> error ">= operator applied to non-numeric values"
    where
        v1 = evaluate' c e1
        v2 = evaluate' c e2
evaluate' c (UXLTE e1 e2) = case vlte v1 v2 of
    Just a -> a
    Nothing -> error "<= operator applied to non-numeric values"
    where
        v1 = evaluate' c e1
        v2 = evaluate' c e2
        
-- boolean operations
evaluate' c (UXAnd e1 e2) = case vand v1 v2 of
    Just a -> a
    Nothing -> error "&& operator applied to non-numeric values"
    where
        v1 = evaluate' c e1
        v2 = evaluate' c e2
evaluate' c (UXOr e1 e2) = case vor v1 v2 of
    Just a -> a
    Nothing -> error "|| operator applied to non-numeric values"
    where
        v1 = evaluate' c e1
        v2 = evaluate' c e2
evaluate' c (UXNot e) = case vnot v of
    Just a -> a
    Nothing -> error "not operator applied to non-numeric value"
    where
        v = evaluate' c e
