module Lib where

import GHC.TypeLits

someFunc :: IO ()
someFunc = putStrLn "someFunc"

data V (s :: Symbol) (t :: Ty) where
    V :: V s t

data Ty = IntT | LolliT Ty Ty

type family AddToCtx (x :: Symbol) (t :: Ty) (xs :: [(Symbol,Ty)]) :: [(Symbol,Ty)] where
    AddToCtx x t '[] = '(x,t) ': '[]
    AddToCtx x t ('(x,t') ': xs) = TypeError (Text "Already in Context")
    AddToCtx x t (y ': xs) = y ': AddToCtx x t xs

type family RemFromCtx (x :: Symbol) (t :: Ty) (xs :: [(Symbol,Ty)]) :: [(Symbol,Ty)] where
    RemFromCtx x t '[] = '[]
    RemFromCtx x t ('(x,t) ': xs) = xs
    RemFromCtx x t (y ': xs) = y ': RemFromCtx x t xs

--type family Union (xs :: [Symbol]) (ys :: [Symbol]) :: [Symbol] where

data Expr (xs :: [(Symbol,Ty)]) (ys :: [(Symbol,Ty)]) (t :: Ty) where
    Var :: (KnownSymbol s, RemFromCtx s t xs ~ ys) => V s t -> Expr xs ys t
    Plus :: Expr xs zs IntT -> Expr zs rs IntT -> Expr xs rs IntT
    Abs :: (KnownSymbol x) => V x s -> Expr (AddToCtx x s xs) ys t -> Expr xs ys (LolliT s t)
    App :: Expr xs zs (LolliT a b) -> Expr zs rs a -> Expr xs rs b

--add :: Expr '[] '[] ('LolliT 'IntT ('LolliT 'IntT 'IntT))
add = Abs @"x" @IntT V (Abs @"y" @IntT V (Plus (Var (V @"z")) (Var (V @"y"))))

data Proxy (a :: k)

check :: Expr '[] '[] t -> Proxy zs
check = undefined

type family Domain (t :: Ty) :: * where
    Domain IntT = Int


--eval :: [(String, )] -> Expr xs ys t -> Domain t
--eval = _
