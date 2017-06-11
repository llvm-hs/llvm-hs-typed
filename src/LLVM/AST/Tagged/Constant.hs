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
{-# LANGUAGE GADTs #-}
{-# LANGUAGE UndecidableInstances #-}

-- | This module provides a type-safe variant of "LLVM.AST.Constant".
module LLVM.AST.Tagged.Constant where

import Data.Word
import GHC.TypeLits
import GHC.Exts (Constraint)

import LLVM.AST.TypeLevel.Type
import LLVM.AST.Type
import LLVM.AST.TypeLevel.Utils
import LLVM.AST.Tagged.Tag
import LLVM.AST.Constant
import LLVM.AST.Name (Name)
import LLVM.AST.Float (SomeFloat)
import LLVM.AST.IntegerPredicate (IntegerPredicate)
import LLVM.AST.FloatingPointPredicate (FloatingPointPredicate)

import Data.Coerce

int :: forall width. Known width => Integer -> Constant ::: IntegerType' width
int value = assertLLVMType $ Int (word32Val @width) value

float :: forall fpt. SomeFloat :::: fpt -> Constant ::: FloatingPointType' fpt
float = coerce Float

null :: forall as t. Known t => Constant ::: PointerType' t as
null = coerce Null (val @_ @t)

struct :: forall b ts. Known b =>
    Maybe Name -> Constant :::* ts -> Constant ::: (StructureType' b ts)
struct mbName xs = coerce Struct mbName (val @_ @b) xs

array :: forall n t. Known t =>
    n × (Constant ::: t) -> Constant ::: (ArrayType' n t)
array vals = coerce Array (val @_ @t) (unCounted vals)

vector :: forall n t. Known t =>
    n × (Constant ::: t) -> Constant ::: (VectorType' n t)
vector vals = coerce Vector (unCounted vals)

undef :: forall t. Known t => Constant ::: t
undef = coerce Undef (val @_ @t)

-- TODO: Does it make sense to include BlockAddress here?

globalReference :: forall t. Known t => Name ::: t -> Constant ::: t
globalReference name = coerce GlobalReference (val @_ @t) name

tokenNone :: Constant ::: TokenType'
tokenNone = coerce TokenNone

add :: forall width.
    Bool -> Bool ->
    Constant ::: IntegerType' width -> Constant ::: IntegerType' width ->
    Constant ::: IntegerType' width
add = coerce Add

fadd :: forall fpt.
    Constant ::: (FloatingPointType' fpt) -> Constant ::: (FloatingPointType' fpt) ->
    Constant ::: (FloatingPointType' fpt)
fadd = coerce FAdd

sub :: forall width.
    Bool -> Bool ->
    Constant ::: IntegerType' width -> Constant ::: IntegerType' width ->
    Constant ::: IntegerType' width
sub = coerce Sub

fsub :: forall fpt.
    Constant ::: (FloatingPointType' fpt) -> Constant ::: (FloatingPointType' fpt) ->
    Constant ::: (FloatingPointType' fpt)
fsub = coerce FSub

mul :: forall width.
    Bool -> Bool ->
    Constant ::: IntegerType' width -> Constant ::: IntegerType' width ->
    Constant ::: IntegerType' width
mul = coerce Mul

fmul :: forall fpt.
    Constant ::: (FloatingPointType' fpt) -> Constant ::: (FloatingPointType' fpt) ->
    Constant ::: (FloatingPointType' fpt)
fmul = coerce FMul

udiv :: forall width.
    Bool ->
    Constant ::: IntegerType' width -> Constant ::: IntegerType' width ->
    Constant ::: IntegerType' width
udiv = coerce UDiv

sdiv :: forall width.
    Bool ->
    Constant ::: IntegerType' width -> Constant ::: IntegerType' width ->
    Constant ::: IntegerType' width
sdiv = coerce SDiv

fdiv :: forall fpt.
    Constant ::: (FloatingPointType' fpt) -> Constant ::: (FloatingPointType' fpt) ->
    Constant ::: (FloatingPointType' fpt)
fdiv = coerce FDiv

urem :: forall width.
    Bool ->
    Constant ::: IntegerType' width -> Constant ::: IntegerType' width ->
    Constant ::: IntegerType' width
urem = coerce UDiv

srem :: forall width.
    Bool ->
    Constant ::: IntegerType' width -> Constant ::: IntegerType' width ->
    Constant ::: IntegerType' width
srem = coerce SDiv

frem :: forall fpt.
    Constant ::: (FloatingPointType' fpt) -> Constant ::: (FloatingPointType' fpt) ->
    Constant ::: (FloatingPointType' fpt)
frem = coerce FDiv

shl :: forall width.
    Bool -> Bool ->
    Constant ::: IntegerType' width -> Constant ::: IntegerType' width ->
    Constant ::: IntegerType' width
shl = coerce Shl

lshr :: forall width.
    Bool ->
    Constant ::: IntegerType' width -> Constant ::: IntegerType' width ->
    Constant ::: IntegerType' width
lshr = coerce LShr

ashr :: forall width.
    Bool ->
    Constant ::: IntegerType' width -> Constant ::: IntegerType' width ->
    Constant ::: IntegerType' width
ashr = coerce AShr

and :: forall width.
    Constant ::: IntegerType' width -> Constant ::: IntegerType' width ->
    Constant ::: IntegerType' width
and = coerce And

or :: forall width.
    Constant ::: IntegerType' width -> Constant ::: IntegerType' width ->
    Constant ::: IntegerType' width
or = coerce Or

xor :: forall width.
    Constant ::: IntegerType' width -> Constant ::: IntegerType' width ->
    Constant ::: IntegerType' width
xor = coerce Xor

-- A list of arguments to @getElementPtr@ which, on the type level,
-- tracks which arguments are statically known.
data GEP_Args (static_args :: [Maybe Nat])  where
    None     :: GEP_Args '[]
    -- | Statically known index. Only this is allowed to index into a structure
    AKnown   :: forall n xs. KnownNat n =>
        GEP_Args xs ->
        GEP_Args (Just n : xs)
    -- | Dynamically known index.
    AUnknown :: forall width xs.
        Constant ::: IntegerType' width ->
        GEP_Args xs ->
        GEP_Args (Nothing : xs)

-- | This type family calculates the return type of a 'getElementPtr' instruction.
type family GEP_Res (t :: Type') (as :: [Maybe nat]) :: Type' where
    GEP_Res t '[] = t
    GEP_Res (StructureType' _ ts) (Just n : as) = GEP_Res (Nth ts n) as
    GEP_Res (PointerType' t2 _)   (_ : as) = GEP_Res t2 as
    GEP_Res (ArrayType' _ t2)     (_ : as) = GEP_Res t2 as


getGEPArgs :: forall static_args. GEP_Args static_args -> [Constant]
getGEPArgs None = []
getGEPArgs (AKnown as) =
    let i :: forall n xs . (Just n : xs) ~ static_args => Integer
            -- this extracts the n from the static args
        i = val @_ @n
    in Int (word32Val @32) i : getGEPArgs as
getGEPArgs (AUnknown v as) = unTyped v : getGEPArgs as

getElementPtr :: forall t as static_args t2.
    Bool ->
    Constant ::: PointerType' t as ->
    GEP_Args static_args ->
    Constant ::: GEP_Res (PointerType' t as) static_args
getElementPtr in_bounds address indices
    = assertLLVMType $ GetElementPtr in_bounds (unTyped address) (getGEPArgs indices)

trunc :: forall width1 width2. (Known width2, width2 <= width1) =>
    Constant ::: IntegerType' width1 -> Constant ::: IntegerType' width2
trunc o1 = coerce Trunc o1 (val @_ @(IntegerType' width2))

zext :: forall width1 width2. (Known width2, width1 <= width2) =>
    Constant ::: IntegerType' width1 -> Constant ::: IntegerType' width2
zext o1 = coerce ZExt o1 (val @_ @(IntegerType' width2))

sext :: forall width1 width2. (Known width2, width1 <= width2) =>
    Constant ::: IntegerType' width1 -> Constant ::: IntegerType' width2
sext o1 = coerce SExt o1 (val @_ @(IntegerType' width2))

fptoui :: forall fpt width. Known width =>
    Constant ::: FloatingPointType' fpt ->
    Constant ::: IntegerType' width
fptoui o1 = coerce FPToUI o1 (val @_ @(IntegerType' width))

fptosi :: forall fpt width. Known width =>
    Constant ::: FloatingPointType' fpt ->
    Constant ::: IntegerType' width
fptosi o1 = coerce FPToSI o1 (val @_ @(IntegerType' width))

uitofp :: forall width fpt. Known fpt =>
    Constant ::: IntegerType' width ->
    Constant ::: FloatingPointType' fpt
uitofp o1 = coerce UIToFP o1 (val @_ @(FloatingPointType' fpt))

sitofp :: forall width fpt. Known fpt =>
    Constant ::: IntegerType' width ->
    Constant ::: FloatingPointType' fpt
sitofp o1 = coerce SIToFP o1 (val @_ @(FloatingPointType' fpt))

fptrunc :: forall fpt1 fpt2.
    (Known fpt2, BitSizeOfFP fpt2 <= BitSizeOfFP fpt1) =>
    Constant ::: FloatingPointType' fpt1 ->
    Constant ::: FloatingPointType' fpt2
fptrunc o1 = coerce FPTrunc o1 (val @_ @(FloatingPointType' fpt2))

fpext :: forall fpt1 fpt2. Known fpt2 =>
    (Known fpt2, BitSizeOfFP fpt1 <= BitSizeOfFP fpt2) =>
    Constant ::: FloatingPointType' fpt1 ->
    Constant ::: FloatingPointType' fpt2
fpext o1 = coerce FPExt o1 (val @_ @(FloatingPointType' fpt2))

ptrtoint :: forall as t width. Known width =>
    Constant ::: PointerType' t as ->
    Constant ::: IntegerType' width
ptrtoint o1 = coerce PtrToInt o1 (val @_ @(IntegerType' width))

inttoptr :: forall as t width. (Known t, Known as) =>
    Constant ::: IntegerType' width ->
    Constant ::: PointerType' t as
inttoptr o1 = coerce IntToPtr o1 (val @_ @(PointerType' t as))

-- | We differentiate between bitcasting non-pointers and bitcasting pointers;
-- there is little point in trying to use one function for these two distinct usecases.
bitcast :: forall t1 t2.
    (Known t2, NonAggregate t1, NonAggregate t2, BitSizeOf t1 ~ BitSizeOf t2) =>
    Constant ::: t1 -> Constant ::: t2
bitcast o1 = coerce BitCast o1 (val @_ @t2)

bitcastPtr :: forall t1 t2 as.
    (Known as, Known t2) =>
    Constant ::: (PointerType' t1 as) -> Constant ::: (PointerType' t2 as)
bitcastPtr o1 = coerce BitCast o1 (val @_ @(PointerType' t2 as))

icmp :: forall width.
    IntegerPredicate ->
    Constant ::: IntegerType' width -> Constant ::: IntegerType' width ->
    Constant ::: IntegerType' width
icmp = coerce ICmp

fcmp :: forall width.
    FloatingPointPredicate ->
    Constant ::: IntegerType' width -> Constant ::: IntegerType' width ->
    Constant ::: IntegerType' width
fcmp = coerce FCmp

select :: forall t.
    Constant ::: IntegerType' 1 ->
    Constant ::: t -> Constant ::: t ->
    Constant ::: t
select = coerce Select

extractElement :: forall n t width.
    Constant ::: VectorType' n t ->
    Constant ::: IntegerType' width ->
    Constant ::: t
extractElement = coerce ExtractElement

insertElement :: forall n t width.
    Constant ::: VectorType' n t ->
    Constant ::: t ->
    Constant ::: IntegerType' width ->
    Constant ::: VectorType' n t
insertElement = coerce InsertElement

shuffleVector :: forall n m t.
    Constant ::: VectorType' n t ->
    Constant ::: VectorType' n t ->
    Constant ::: VectorType' m (IntegerType' 32) ->
    Constant ::: VectorType' m t
shuffleVector = coerce ShuffleVector

type family NotNull (xs :: [a]) :: Constraint  where
    NotNull '[] = TypeError (Text "The list must not be empty")
    NotNull _ = ()

type family ValueAt (t :: Type') (as :: [nat]) :: Type' where
    ValueAt t '[] = t
    ValueAt (StructureType' _ ts) (n : as) = ValueAt (Nth ts n) as
    ValueAt (ArrayType' _ t2)     (_ : as) = ValueAt t2 as
    ValueAt t _ = TypeError (Text "Cannot index into non-aggregate type " :$$: ShowType t)

-- | The indices to extractValue need to be known at compile time, to index into
-- structures.
extractValue :: forall t (idxs :: [Nat]).
    (Known idxs, NotNull idxs) =>
    Constant ::: t ->
    Constant ::: ValueAt t idxs
extractValue c = coerce ExtractValue c (map fromIntegral (val @_ @idxs) :: [Word32])

-- | The indices to insertValue need to be known at compile time, to index into
-- structures.
insertValue :: forall t (idxs :: [Nat]).
    (Known idxs, NotNull idxs) =>
    Constant ::: t ->
    Constant ::: ValueAt t idxs ->
    Constant ::: t
insertValue c v = coerce InsertValue c v (map fromIntegral (val @_ @idxs) :: [Word32])
