{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module LLVM.AST.TypeLevel.Utils where

import GHC.TypeLits

type family Nth (xs :: [a]) n :: a where
    Nth '[] 0 = TypeError (Text "empty list")
    Nth (x:xs) 0 = x
    Nth (x:xs) n = Nth xs (n-1)
