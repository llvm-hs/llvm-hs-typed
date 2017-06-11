{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

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
import GHC.Exts (Constraint)
import Data.String.Encode
import qualified Data.ByteString.Short as BS

import LLVM.AST.Type
import LLVM.AST.AddrSpace
import LLVM.AST.Name

data Name' = Name' Symbol | UnName' Nat

data AddrSpace' = AddrSpace' Nat

-- | A copy of 'Type', suitable to be used on the type level
data Type'
  = VoidType'
  | IntegerType' Nat
  | PointerType' Type' AddrSpace'
  | FloatingPointType' FloatingPointType
  | FunctionType' Type' [Type']
    -- ^ we do not support varargs in the typed represenation
  | VectorType' Nat Type'
  | StructureType' Bool [Type']
  | ArrayType' Nat Type'
  | NamedTypeReference' Name'
  | MetadataType'
  | TokenType'
  | LabelType'

-- | Ensures a type is not void
type family NonVoid (t :: Type') :: Constraint where
    NonVoid VoidType' = TypeError (Text "Type must not be void")
    NonVoid t         = ()

-- | A non-aggregate, non-vector type. Basically, everything that can
-- be bitcaste’d into each other.
type family NonAggregate (t :: Type') :: Constraint where
    NonAggregate (IntegerType' _)       = ()
    NonAggregate (FloatingPointType' _) = ()
    NonAggregate (VectorType' _ _ )     = ()
    NonAggregate t = TypeError (ShowType t :<>: Text " is aggregate")

-- | Bit widths of the given floating point type
type family BitSizeOfFP (t :: FloatingPointType) :: Nat where
    BitSizeOfFP HalfFP      = 16
    BitSizeOfFP FloatFP     = 32
    BitSizeOfFP DoubleFP    = 64
    BitSizeOfFP FP128FP     = 128
    BitSizeOfFP X86_FP80FP  = 80
    BitSizeOfFP PPC_FP128FP = 128

-- | Bit widths of this nonaggregate type
type family BitSizeOf (t :: Type') :: Nat where
    BitSizeOf (IntegerType' w)         = w
    BitSizeOf (FloatingPointType' fpf) = BitSizeOfFP fpf
    BitSizeOf (VectorType' n t)        = n * BitSizeOf t
    BitSizeOf t = TypeError (ShowType t :<>: Text " is aggregate")

-- | This type family indicates the value-level representation of a type-level
-- type. Often these are the same.
type family Value k :: *

-- | This class connects type variables (of kind @k@) to their value-level
-- representation (of type 'Value k').
class Known (t :: k)  where
    val :: Value k

type instance Value Type' = Type
type instance Value [a] = [Value a]
type instance Value AddrSpace' = AddrSpace
type instance Value Name' = Name
type instance Value FloatingPointType = FloatingPointType
type instance Value Bool = Bool
type instance Value Symbol = String
type instance Value Nat = Integer

word32Val :: forall (n::Nat). Known n => Word32
word32Val = fromIntegral (val @_ @n)

word64Val :: forall (n::Nat). Known n => Word64
word64Val = fromIntegral (val @_ @n)

wordVal :: forall (n::Nat). Known n => Word
wordVal = fromIntegral (val @_ @n)

byteStringVal :: forall (s::Symbol). Known s => BS.ShortByteString
byteStringVal = convertString (val @_ @s)

instance Known VoidType' where
    val = VoidType
instance Known n => Known (IntegerType' n) where
    val = IntegerType (word32Val @n)
instance (Known t, Known as) => Known (PointerType' t as) where
    val = PointerType (val @_ @t) (val @_ @as)
instance Known fpt => Known (FloatingPointType' fpt) where
    val = FloatingPointType (val @_ @fpt)
instance (Known t, Known ts) => Known (FunctionType' t ts) where
    val = FunctionType (val @_ @t) (val @_ @ts) False
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
instance Known LabelType' where
    val = LabelType

instance Known '[] where
    val = []
instance (Known t, Known tys) => Known (t:tys) where
    val = (val @_ @t) : (val @_ @tys)

instance Known n => Known ('AddrSpace' n) where
    val = AddrSpace (word32Val @n)

instance Known s => Known ('Name' s) where
    val = Name (byteStringVal @s)
instance Known n => Known (UnName' n) where
    val = UnName (wordVal @n)

instance Known HalfFP      where val = HalfFP
instance Known FloatFP     where val = FloatFP
instance Known DoubleFP    where val = DoubleFP
instance Known FP128FP     where val = FP128FP
instance Known X86_FP80FP  where val = X86_FP80FP
instance Known PPC_FP128FP where val = PPC_FP128FP

instance Known True        where val = True
instance Known False       where val = False

instance KnownNat n => Known (n :: Nat) where
    val = natVal @n undefined
instance KnownSymbol s => Known (s :: Symbol) where
    val = symbolVal @s undefined
