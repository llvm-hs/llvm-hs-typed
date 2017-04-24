{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

{- |
This modules contains a variant of the 'LLVM.AST.Type.Type' type, suitable for
promotion to the type-level. We cannot just use 'LLVM.AST.Type.Type' directy,
as basic types such as 'Word32' are not useful as kinds, and we have to replace
them with 'GHC.TypeLits.Nat'.

Because we have to define a new type, but expect users want access to both
variants, we have to avoid name clashes. The convention is to simply append a
@'@.

This module also contains various type-level functions, classes and other
machinery that provide the necessary functionality for the typed AST.

-}
module LLVM.AST.TypeLevel.Type where

import Data.Word
import GHC.TypeLits

import LLVM.AST.Type
import LLVM.AST.AddrSpace
import LLVM.AST.Name

data Name' = Name' Symbol | UnName' Nat

data AddrSpace' = AddrSpace' Nat

data Type'
  = VoidType'
  | IntegerType' Nat
  | PointerType' Type' AddrSpace'
  | FloatingPointType' Nat FloatingPointFormat
  | FunctionType' Type' [Type'] Bool
  | VectorType' Nat Type'
  | StructureType' Bool [Type']
  | ArrayType' Nat Type'
  | NamedTypeReference' Name'
  | MetadataType'
  | TokenType'

-- TODO: Can we have one (kind) class to subsume the following repetition?

type family Value k :: *
class Known (t :: k)  where
    val :: Value k

type instance Value Type' = Type
type instance Value [a] = [Value a]
type instance Value AddrSpace' = AddrSpace
type instance Value Name' = Name
type instance Value FloatingPointFormat = FloatingPointFormat
type instance Value Bool = Bool
type instance Value Symbol = String
type instance Value Nat = Integer

word32Val :: forall (n::Nat). Known n => Word32
word32Val = fromIntegral (val @_ @n)

word64Val :: forall (n::Nat). Known n => Word64
word64Val = fromIntegral (val @_ @n)

wordVal :: forall (n::Nat). Known n => Word
wordVal = fromIntegral (val @_ @n)

instance Known VoidType' where
    val = VoidType
instance Known n => Known (IntegerType' n) where
    val = IntegerType (word32Val @n)
instance (Known t, Known as) => Known (PointerType' t as) where
    val = PointerType (val @_ @t) (val @_ @as)
instance (Known n, Known fpf) => Known (FloatingPointType' n fpf) where
    val = FloatingPointType (word32Val @n) (val @_ @fpf)
instance (Known t, Known ts, Known b) => Known (FunctionType' t ts b) where
    val = FunctionType (val @_ @t) (val @_ @ts) (val @_ @b)
instance (Known n, Known t) => Known (VectorType' n t) where
    val = VectorType (word32Val @n) (val @_ @t)
instance (Known b, Known ts) => Known (StructureType' b ts) where
    val = StructureType (val @_ @b) (val @_ @ts)
instance (Known n, Known t) => Known (ArrayType' n t) where
    val = ArrayType (word64Val @n) (val @_ @t)
instance Known n => Known (NamedTypeReference' n) where
    val = NamedTypeReference (val @_ @n)
instance Known MetadataType' where
    val = MetadataType
instance Known TokenType' where
    val = TokenType

instance Known '[] where
    val = []
instance (Known t, Known tys) => Known (t:tys) where
    val = (val @_ @t) : (val @_ @tys)

instance Known n => Known ('AddrSpace' n) where
    val = AddrSpace (word32Val @n)

instance Known s => Known ('Name' s) where
    val = Name (val @_ @s)
instance Known n => Known (UnName' n) where
    val = UnName (wordVal @n)

instance Known IEEE where
    val = IEEE
instance Known DoubleExtended where
    val = DoubleExtended
instance Known PairOfFloats where
    val = PairOfFloats

instance Known True where
    val = True
instance Known False where
    val = False

instance KnownNat n => Known (n :: Nat) where
    val = natVal @n undefined
instance KnownSymbol s => Known (s :: Symbol) where
    val = symbolVal @s undefined
