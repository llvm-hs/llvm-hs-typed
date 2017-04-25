{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}

module LLVM.AST.Tagged.Tag where

import LLVM.AST.TypeLevel.Type

-- | A value of type @v ::: t@ denotes a value of type @v@ with an LLVM type
-- annotation of @t :: Type'@.
--
-- This anntation exists only on the Haskell type level. Functions that need to
-- get hold of the actual 'LLVM.Ast.Type.Type' associated to the tag will
-- typically have a 'Known' type class constraint.
newtype v ::: (t :: Type') = Typed v

-- | Adds an LLVM type annotation to its argument. Note that this function is unchecked.
assertLLVMType :: v -> v ::: t
assertLLVMType = Typed

-- | Removes the LLVM type annotation.
unTyped :: v ::: t -> v
unTyped (Typed v) = v
