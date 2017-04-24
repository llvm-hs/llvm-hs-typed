{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

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

class KnownType (t :: Type') where
    typeVal :: Type

class KnownTypes (t :: [Type']) where
    typesVal :: [Type]

class KnownAddrSpace (t :: AddrSpace') where
    addrSpaceVal :: AddrSpace

class KnownName (t :: Name') where
    nameVal :: Name

class KnownFloatingPointFormat (fpf :: FloatingPointFormat) where
    floatingPointFormatVal :: FloatingPointFormat

class KnownBool (b :: Bool) where
    boolVal :: Bool

word32Val :: forall n. KnownNat n => Word32
word32Val = fromIntegral (natVal @n undefined)

word64Val :: forall n. KnownNat n => Word64
word64Val = fromIntegral (natVal @n undefined)

wordVal :: forall n. KnownNat n => Word
wordVal = fromIntegral (natVal @n undefined)

instance KnownType VoidType' where
    typeVal = VoidType
instance KnownNat n => KnownType (IntegerType' n) where
    typeVal = IntegerType (word32Val @n)
instance (KnownType t, KnownAddrSpace as) => KnownType (PointerType' t as) where
    typeVal = PointerType (typeVal @t) (addrSpaceVal @as)
instance (KnownNat n, KnownFloatingPointFormat fpf) => KnownType (FloatingPointType' n fpf) where
    typeVal = FloatingPointType (word32Val @n) (floatingPointFormatVal @fpf)
instance (KnownType t, KnownTypes ts, KnownBool b) => KnownType (FunctionType' t ts b) where
    typeVal = FunctionType (typeVal @t) (typesVal @ts) (boolVal @b)
instance (KnownNat n, KnownType t) => KnownType (VectorType' n t) where
    typeVal = VectorType (word32Val @n) (typeVal @t)
instance (KnownBool b, KnownTypes ts) => KnownType (StructureType' b ts) where
    typeVal = StructureType (boolVal @b) (typesVal @ts)
instance (KnownNat n, KnownType t) => KnownType (ArrayType' n t) where
    typeVal = ArrayType (word64Val @n) (typeVal @t)
instance KnownName n => KnownType (NamedTypeReference' n) where
    typeVal = NamedTypeReference (nameVal @n)
instance KnownType MetadataType' where
    typeVal = MetadataType
instance KnownType TokenType' where
    typeVal = TokenType

instance KnownTypes '[] where
    typesVal = []
instance (KnownType t, KnownTypes tys) => KnownTypes (t:tys) where
    typesVal = (typeVal @t) : (typesVal @tys)

instance KnownNat n => KnownAddrSpace ('AddrSpace' n) where
    addrSpaceVal = AddrSpace (word32Val @n)

instance KnownSymbol s => KnownName ('Name' s) where
    nameVal = Name (symbolVal @s undefined)
instance KnownNat n => KnownName (UnName' n) where
    nameVal = UnName (wordVal @n)

instance KnownFloatingPointFormat IEEE where
    floatingPointFormatVal = IEEE
instance KnownFloatingPointFormat DoubleExtended where
    floatingPointFormatVal = DoubleExtended
instance KnownFloatingPointFormat PairOfFloats where
    floatingPointFormatVal = PairOfFloats

instance KnownBool True where
    boolVal = True
instance KnownBool False where
    boolVal = False
