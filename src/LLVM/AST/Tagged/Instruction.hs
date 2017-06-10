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

-- | This module provides a type-safe variant of "LLVM.AST.Instruction".
-- It is currently a stub
module LLVM.AST.Tagged.Instruction where

import LLVM.AST.Tagged.Tag
import LLVM.AST.TypeLevel.Type
import LLVM.AST.Instruction
import LLVM.AST.Tagged.Name

-- TODO: Lots of stuff missing

-- | This is the type-safe type corresponding to @Named Instruction@. It
-- enforces that an instruction has a name if and only if it is not a void
-- instruction, and that the name and instruction have the same type.
name :: forall (t :: Type').  NonVoid t =>
    Name ::: t ->
    Instruction ::: t ->
    Named Instruction ::: t
name tn ti = assertLLVMType $ unTyped tn := unTyped ti

-- | If you do have a void instruction, you must use 'do'' and not pass a name
-- to it.
do' :: Instruction ::: VoidType' -> Named Instruction ::: VoidType'
do' ti = assertLLVMType $ Do (unTyped ti)

