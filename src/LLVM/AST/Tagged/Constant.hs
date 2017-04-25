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

-- | This module provides a type-safe variant of "LLVM.AST.Constant".
-- It is currently a stub
module LLVM.AST.Tagged.Constant where

import Data.Word
import GHC.TypeLits

import LLVM.AST.TypeLevel.Type
import LLVM.AST.Tagged.Tag
import LLVM.AST.Constant
import LLVM.AST.Name (Name)
import LLVM.AST.Float (SomeFloat)
import LLVM.AST.IntegerPredicate (IntegerPredicate)
import LLVM.AST.FloatingPointPredicate (FloatingPointPredicate)

int :: forall width. Known width => Integer -> Constant ::: IntegerType' width
int value = assertLLVMType $ Int (word32Val @width) value

-- TODO: Tie the width to the width of the SomeFloat
float :: forall width fpf. SomeFloat -> Constant ::: FloatingPointType' width fpf
float someFloat = assertLLVMType $ Float someFloat

null :: forall as t. Known t => Constant ::: PointerType' t as
null = assertLLVMType $ Null (val @_ @t)

struct :: forall b ts. Known b =>
    Maybe Name -> Constant :::* ts -> Constant ::: (StructureType' b ts)
struct mbName xs = assertLLVMType $ Struct mbName (val @_ @b) (unTypeds xs)

-- TODO: Fix the number of elements
array :: forall n t. Known t => [Constant ::: t] -> Constant ::: (ArrayType' n t)
array vals = assertLLVMType $ Array (val @_ @t) (map unTyped vals)

-- TODO: Fix the number of elements
vector :: forall n t. Known t => [Constant ::: t] -> Constant ::: (VectorType' n t)
vector vals = assertLLVMType $ Vector (map unTyped vals)

undef :: forall t. Known t => Constant ::: t
undef = assertLLVMType $ Undef (val @_ @t)

-- TODO: Does it make sense to include BlockAddress here?

globalReference :: forall t. Known t => Name ::: t -> Constant ::: t
globalReference name = assertLLVMType $ GlobalReference (val @_ @t) (unTyped name)

tokenNone :: Constant ::: TokenType'
tokenNone = assertLLVMType $ TokenNone

add :: forall width.
    Bool -> Bool ->
    Constant ::: IntegerType' width -> Constant ::: IntegerType' width ->
    Constant ::: IntegerType' width
add nsw nuw o1 o2 = assertLLVMType $ Add nsw nuw (unTyped o1) (unTyped o2)

fadd :: forall width fpf.
    Constant ::: (FloatingPointType' width fpf) -> Constant ::: (FloatingPointType' width fpf) ->
    Constant ::: (FloatingPointType' width fpf)
fadd o1 o2 = assertLLVMType $ FAdd (unTyped o1) (unTyped o2)

sub :: forall width.
    Bool -> Bool ->
    Constant ::: IntegerType' width -> Constant ::: IntegerType' width ->
    Constant ::: IntegerType' width
sub nsw nuw o1 o2 = assertLLVMType $ Sub nsw nuw (unTyped o1) (unTyped o2)


fsub :: forall width fpf.
    Constant ::: (FloatingPointType' width fpf) -> Constant ::: (FloatingPointType' width fpf) ->
    Constant ::: (FloatingPointType' width fpf)
fsub o1 o2 = assertLLVMType $ FSub (unTyped o1) (unTyped o2)


mul :: forall width.
    Bool -> Bool ->
    Constant ::: IntegerType' width -> Constant ::: IntegerType' width ->
    Constant ::: IntegerType' width
mul nsw nuw o1 o2 = assertLLVMType $ Mul nsw nuw (unTyped o1) (unTyped o2)

fmul :: forall width fpf.
    Constant ::: (FloatingPointType' width fpf) -> Constant ::: (FloatingPointType' width fpf) ->
    Constant ::: (FloatingPointType' width fpf)
fmul o1 o2 = assertLLVMType $ FMul (unTyped o1) (unTyped o2)


udiv :: forall width.
    Bool ->
    Constant ::: IntegerType' width -> Constant ::: IntegerType' width ->
    Constant ::: IntegerType' width
udiv exact o1 o2 = assertLLVMType $ UDiv exact (unTyped o1) (unTyped o2)

sdiv :: forall width.
    Bool ->
    Constant ::: IntegerType' width -> Constant ::: IntegerType' width ->
    Constant ::: IntegerType' width
sdiv exact o1 o2 = assertLLVMType $ SDiv exact (unTyped o1) (unTyped o2)

fdiv :: forall width fpf.
    Constant ::: (FloatingPointType' width fpf) -> Constant ::: (FloatingPointType' width fpf) ->
    Constant ::: (FloatingPointType' width fpf)
fdiv o1 o2 = assertLLVMType $ FDiv (unTyped o1) (unTyped o2)


urem :: forall width.
    Bool ->
    Constant ::: IntegerType' width -> Constant ::: IntegerType' width ->
    Constant ::: IntegerType' width
urem exact o1 o2 = assertLLVMType $ UDiv exact (unTyped o1) (unTyped o2)

srem :: forall width.
    Bool ->
    Constant ::: IntegerType' width -> Constant ::: IntegerType' width ->
    Constant ::: IntegerType' width
srem exact o1 o2 = assertLLVMType $ SDiv exact (unTyped o1) (unTyped o2)

frem :: forall width fpf.
    Constant ::: (FloatingPointType' width fpf) -> Constant ::: (FloatingPointType' width fpf) ->
    Constant ::: (FloatingPointType' width fpf)
frem o1 o2 = assertLLVMType $ FDiv (unTyped o1) (unTyped o2)


shl :: forall width.
    Bool -> Bool ->
    Constant ::: IntegerType' width -> Constant ::: IntegerType' width ->
    Constant ::: IntegerType' width
shl nsw nuw o1 o2 = assertLLVMType $ Shl nsw nuw (unTyped o1) (unTyped o2)

lshr :: forall width.
    Bool ->
    Constant ::: IntegerType' width -> Constant ::: IntegerType' width ->
    Constant ::: IntegerType' width
lshr exact o1 o2 = assertLLVMType $ LShr exact (unTyped o1) (unTyped o2)

ashr :: forall width.
    Bool ->
    Constant ::: IntegerType' width -> Constant ::: IntegerType' width ->
    Constant ::: IntegerType' width
ashr exact o1 o2 = assertLLVMType $ AShr exact (unTyped o1) (unTyped o2)

and :: forall width.
    Constant ::: IntegerType' width -> Constant ::: IntegerType' width ->
    Constant ::: IntegerType' width
and o1 o2 = assertLLVMType $ And (unTyped o1) (unTyped o2)

or :: forall width.
    Constant ::: IntegerType' width -> Constant ::: IntegerType' width ->
    Constant ::: IntegerType' width
or o1 o2 = assertLLVMType $ Or (unTyped o1) (unTyped o2)

xor :: forall width.
    Constant ::: IntegerType' width -> Constant ::: IntegerType' width ->
    Constant ::: IntegerType' width
xor o1 o2 = assertLLVMType $ Xor (unTyped o1) (unTyped o2)

-- TODO: Use type system machinery to fix t2, and to say something about the types of indices
getElementPtr :: forall t as t2.
    Bool ->
    Constant ::: (PointerType' t as) ->
    [Constant] ->
    Constant ::: t2
getElementPtr in_bounds address indices = assertLLVMType $ GetElementPtr in_bounds (unTyped address) indices

trunc :: forall width1 width2. (Known width2, width2 <= width1) =>
    Constant ::: IntegerType' width1 -> Constant ::: IntegerType' width2
trunc o1 = assertLLVMType $ Trunc (unTyped o1) (val @_ @(IntegerType' width2))

zext :: forall width1 width2. (Known width2, width1 <= width2) =>
    Constant ::: IntegerType' width1 -> Constant ::: IntegerType' width2
zext o1 = assertLLVMType $ ZExt (unTyped o1) (val @_ @(IntegerType' width2))

sext :: forall width1 width2. (Known width2, width1 <= width2) =>
    Constant ::: IntegerType' width1 -> Constant ::: IntegerType' width2
sext o1 = assertLLVMType $ SExt (unTyped o1) (val @_ @(IntegerType' width2))

fptoui :: forall width1 width2 fpf. Known width2 =>
    Constant ::: FloatingPointType' width1 fpf ->
    Constant ::: IntegerType' width2
fptoui o1 = assertLLVMType $ FPToUI (unTyped o1) (val @_ @(IntegerType' width2))

fptosi :: forall width1 width2 fpf. Known width2 =>
    Constant ::: FloatingPointType' width1 fpf ->
    Constant ::: IntegerType' width2
fptosi o1 = assertLLVMType $ FPToSI (unTyped o1) (val @_ @(IntegerType' width2))

uitofp :: forall width1 width2 fpf. (Known width2, Known fpf) =>
    Constant ::: IntegerType' width1 ->
    Constant ::: FloatingPointType' width2 fpf
uitofp o1 = assertLLVMType $ UIToFP (unTyped o1) (val @_ @(FloatingPointType' width2 fpf))

sitofp :: forall width1 width2 fpf. (Known width2, Known fpf) =>
    Constant ::: IntegerType' width1 ->
    Constant ::: FloatingPointType' width2 fpf
sitofp o1 = assertLLVMType $ SIToFP (unTyped o1) (val @_ @(FloatingPointType' width2 fpf))


-- TODO: Are the FloatingPointFormats going to be the same?
fptrunc :: forall width1 width2 fpf1 fpf2. (Known width2, Known fpf2, width2 <= width1)=>
    Constant ::: FloatingPointType' width1 fpf1 ->
    Constant ::: FloatingPointType' width2 fpf2
fptrunc o1 = assertLLVMType $ FPTrunc (unTyped o1) (val @_ @(FloatingPointType' width2 fpf2))

-- TODO: Are the FloatingPointFormats going to be the same?
fpext :: forall width1 width2 fpf1 fpf2. (Known width2, Known fpf2, width1 <= width2)=>
    Constant ::: FloatingPointType' width1 fpf1 ->
    Constant ::: FloatingPointType' width2 fpf2
fpext o1 = assertLLVMType $ FPExt (unTyped o1) (val @_ @(FloatingPointType' width2 fpf2))

ptrtoint :: forall as t width. Known width =>
    Constant ::: PointerType' t as ->
    Constant ::: IntegerType' width
ptrtoint o1 = assertLLVMType $ PtrToInt (unTyped o1) (val @_ @(IntegerType' width))

inttoptr :: forall as t width. (Known t, Known as) =>
    Constant ::: IntegerType' width ->
    Constant ::: PointerType' t as
inttoptr o1 = assertLLVMType $ IntToPtr (unTyped o1) (val @_ @(PointerType' t as))

-- TODO: bitcast has many rules about its argument. Implement them!
bitcast :: forall t1 t2. Known t2 =>
    Constant ::: t1 -> Constant ::: t2
bitcast o1 = assertLLVMType $ BitCast (unTyped o1) (val @_ @t2)

icmp :: forall width.
    IntegerPredicate ->
    Constant ::: IntegerType' width -> Constant ::: IntegerType' width ->
    Constant ::: IntegerType' width
icmp pred o1 o2 = assertLLVMType $ ICmp pred (unTyped o1) (unTyped o2)

fcmp :: forall width.
    FloatingPointPredicate ->
    Constant ::: IntegerType' width -> Constant ::: IntegerType' width ->
    Constant ::: IntegerType' width
fcmp pred o1 o2 = assertLLVMType $ FCmp pred (unTyped o1) (unTyped o2)

select :: forall t.
    Constant ::: IntegerType' 1 ->
    Constant ::: t -> Constant ::: t ->
    Constant ::: t
select pred o1 o2 = assertLLVMType $ Select (unTyped pred) (unTyped o1) (unTyped o2)

extractElement :: forall n t width.
    Constant ::: VectorType' n t ->
    Constant ::: IntegerType' width ->
    Constant ::: t
extractElement vector index = assertLLVMType $ ExtractElement (unTyped vector) (unTyped index)

insertElement :: forall n t width.
    Constant ::: VectorType' n t ->
    Constant ::: t ->
    Constant ::: IntegerType' width ->
    Constant ::: VectorType' n t
insertElement vector element index = assertLLVMType $ InsertElement (unTyped vector) (unTyped element) (unTyped index)

shuffleVector :: forall n m t.
    Constant ::: VectorType' n t ->
    Constant ::: VectorType' n t ->
    Constant ::: VectorType' m (IntegerType' 32) ->
    Constant ::: VectorType' m t
shuffleVector o1 o2 mask = assertLLVMType $ ShuffleVector (unTyped o1) (unTyped o2) (unTyped mask)

-- TODO: add indices to the type level, calculate t2
extractValue :: forall t t2.
    Constant ::: t ->
    [Word32] ->
    Constant ::: t2
extractValue aggregate indices = assertLLVMType $ ExtractValue (unTyped aggregate) indices

-- TODO: add indices to the type level, calculate t2
insertValue :: forall t t2.
    Constant ::: t ->
    Constant ::: t2 ->
    [Word32] ->
    Constant ::: t
insertValue aggregate element indices = assertLLVMType $ InsertValue (unTyped aggregate) (unTyped element) indices
