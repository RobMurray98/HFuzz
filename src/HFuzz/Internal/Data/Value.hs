-----------------------------------------------------------------------------
-- |
-- Module      :  HFuzz.Internal.Data.Value
-- Description :  Value data type for interpretation of expressions
-- Copyright   :  (c) Robert Murray
-- License     :  MIT
--
-- Maintainer  :  R.Murray.1@warwick.ac.uk
-- Stability   :  experimental
--
-- Module providing a data type Value that is used to hold values during
-- interpretation of expressions, along with methods that operate on these.
--
-----------------------------------------------------------------------------

module HFuzz.Internal.Data.Value (
    Value(..),
    vplus,
    vminus,
    vtimes,
    veq,
    vlt,
    vgt,
    vlte,
    vgte,
    vand,
    vor,
    vnot,
    castToNum,
    castToInt,
    castToBool
    ) where

data Value where
    VBool :: Bool -> Value
    VInt :: Int -> Value
    VNum :: Double -> Value
    VString :: String -> Value
    VList :: [Value] -> Value
    VPair :: Value -> Value -> Value
    VUnit :: Value

instance Show Value where
    show (VBool a) = show a
    show (VInt a) = show a
    show (VNum a) = show a
    show (VString a) = show a
    show (VList xs) = show xs
    show (VPair x y) = "(" ++ show x ++ ", " ++ show y ++ ")"
    show VUnit = "()"

instance Eq Value where
    (VBool a) == (VBool b) = a == b
    (VInt a) == (VInt b) = a == b
    (VNum a) == (VNum b) = a == b
    (VString a) == (VString b) = a == b
    (VList a) == (VList b) = a == b
    (VPair a b) == (VPair c d) = (a == c) && (b == d)
    VUnit == VUnit = True

vplus = vop $ c VNum (+)
vminus = vop $ c VNum (-)
vtimes = vop $ c VNum (*)
veq = vop $ c VBool (==)
vlt = vop $ c VBool (<)
vgt = vop $ c VBool (>)
vlte = vop $ c VBool (<=)
vgte = vop $ c VBool (>=)

vand = vbop $ c VBool (&&)
vor = vbop $ c VBool (||)
vnot = vubop $ VBool . not

castToNum = vuop $ VNum . id
castToInt = vuiop $ VInt . id
castToBool = vubop $ VBool . id

-- PRIVATE:

c :: (c -> d) -> (a -> b -> c) -> a -> b -> d
c f g x y = f $ g x y 

vop :: (Double -> Double -> Value) -> Value -> Value -> Maybe Value
vop f v1 v2 = do
    a <- getNumValue v1
    b <- getNumValue v2
    return $ f a b

viop :: (Int -> Int -> Value) -> Value -> Value -> Maybe Value
viop f v1 v2 = do
    a <- getIntValue v1
    b <- getIntValue v2
    return $ f a b

vbop :: (Bool -> Bool -> Value) -> Value -> Value -> Maybe Value
vbop f v1 v2 = do
    a <- getBooleanValue v1
    b <- getBooleanValue v2
    return $ f a b

vuop :: (Double -> Value) -> Value -> Maybe Value
vuop f v = do
    a <- getNumValue v
    return $ f a

vuiop :: (Int -> Value) -> Value -> Maybe Value
vuiop f v = do
    a <- getIntValue v
    return $ f a

vubop :: (Bool -> Value) -> Value -> Maybe Value
vubop f v = do
    a <- getBooleanValue v
    return $ f a

getNumValue :: Value -> Maybe Double
getNumValue v = case v of
    (VInt a) -> Just $ fromIntegral a
    (VNum a) -> Just a
    (VBool b) -> case b of
        True -> Just 1.0
        False -> Just 0.0
    _ -> Nothing

getIntValue :: Value -> Maybe Int
getIntValue v = case v of
    (VInt a) -> Just a
    (VNum a) -> Just $ round a
    (VBool b) -> case b of
        True -> Just 1
        False -> Just 0
    _ -> Nothing

getBooleanValue :: Value -> Maybe Bool
getBooleanValue v = case v of
    (VBool b) -> Just b
    (VInt i) -> Just (i /= 0)
    (VNum n) -> Just (n /= 0.0)
    _ -> Nothing