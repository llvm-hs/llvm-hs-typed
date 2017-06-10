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
  -> Named Terminator
  -> BasicBlock
basicBlock = BasicBlock

parameter 
  :: forall t. Known t 
  => (Name ::: t)
  -> [A.ParameterAttribute]
  -> (Parameter :::: t)
parameter nm attrs = assertLLVMType $ Parameter (val @_ @t) (unTyped nm) attrs

function 
  :: forall t. Known t 
  => (Name ::: t)
  -> Type'
  -> ([Parameter :::: t], Bool)
  -> Global
function nm retty (params,variadic) = functionDefaults
  { AST.name = unTyped nm
  , AST.returnType = (val @_ @t)
  , AST.parameters = (fmap unTyped params, variadic)
  }
