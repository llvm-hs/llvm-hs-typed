{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}

module LLVM.AST.Tagged.Tag where

import GHC.TypeLits

import LLVM.AST.TypeLevel.Type

-- | A value of type @v ::: t@ denotes a value of type @v@ with an LLVM type
-- annotation of @t :: Type'@.
--
-- This anntation exists only on the Haskell type level. Functions that need to
-- get hold of the actual 'LLVM.Ast.Type.Type' associated to the tag will
-- typically have a 'Known' type class constraint.
type v ::: (t :: Type') = v :::: t

-- | Sometimes we want to annotate a value @v@ with something else than an LLVM
-- type (@Type'@), so this allows any kind
newtype v :::: (t :: k) = Typed v


-- | Adds an LLVM type annotation to its argument. Note that this function is unchecked.
assertLLVMType :: v -> v :::: t
assertLLVMType = Typed

-- | Removes the LLVM type annotation.
unTyped :: v :::: t -> v
unTyped (Typed v) = v

-- TODO: Can we have a nicer name here

-- | A list of tagged values
data v :::* (ts :: [k']) where
    Nil    ::                          v :::* '[]
    (:*)   :: v :::: t -> v :::* ts -> v :::* (t:ts)
infixr 5 :*

unTypeds :: v :::* ts -> [v]
unTypeds Nil = []
unTypeds (x :* xs) = unTyped x : unTypeds xs

-- | A vector type
data (n::Nat) × a where
    VNil   ::               0 × a
    (:×)   :: a -> n × a -> (1 + n) × a
infixr 5 :×


unCounted :: n × a -> [a]
unCounted VNil = []
unCounted (x :× xs) = x : unCounted xs
