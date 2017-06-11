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
{-# LANGUAGE TypeApplications #-}

-- | This module provides a type-safe variant of "LLVM.AST.Global".
-- It is currently a stub
module LLVM.AST.Tagged.Global (
  basicBlock,
  parameter,
  function,
) where

import Data.Coerce

import LLVM.AST.Name
import LLVM.AST.Global as AST
import qualified LLVM.AST.Attribute as A
import LLVM.AST.Instruction (Named, Instruction, Terminator)

import LLVM.AST.Tagged.Tag
import LLVM.AST.Tagged.Name
import LLVM.AST.Tagged.Instruction
import LLVM.AST.TypeLevel.Type

basicBlock
  :: Name
  -> [Named Instruction]
  -> Named (Terminator ::: t)
  -> (BasicBlock ::: t)
basicBlock nm instr term = assertLLVMType $ BasicBlock nm instr (coerce term)

parameter
  :: forall t. Known t
  => (Name ::: t)
  -> [A.ParameterAttribute]
  -> (Parameter ::: t)
parameter nm attrs = coerce Parameter (val @_ @t) nm attrs

-- | This creates a 'Global' from typed parameters. It is equal to
-- 'functionDefaults' with the fields 'AST.name', 'AST.returnType' and
-- 'AST.parameters' set.
--
-- It does not support varargs.
function
  :: forall ret_ty args_tys as. Known ret_ty
  => (Name ::: PointerType' (FunctionType' ret_ty args_tys False) as)
  -> (Parameter :::*  args_tys, Bool)
  -> [BasicBlock ::: ret_ty]
  -> Global
function nm (params,variadic) bbs = functionDefaults
  { AST.name = coerce nm
  , AST.returnType = (val @_ @ret_ty)
  , AST.parameters = (coerce params, variadic)
  , AST.basicBlocks = (coerce bbs)
  }
