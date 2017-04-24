-- | This module provides a type-safe variant of "LLVM.AST".
-- It is currently a stub
module LLVM.AST.Tagged (
  Module(..), defaultModule,
  Definition(..),
  Global(GlobalVariable, GlobalAlias, Function),
  globalVariableDefaults,
  globalAliasDefaults,
  functionDefaults,
  UnnamedAddr(..),
  Parameter(..),
  BasicBlock(..),
  module LLVM.AST.Tagged.Instruction,
  module LLVM.AST.Tagged.Name,
  module LLVM.AST.Tagged.Operand,
  module LLVM.AST.Tagged.Type
  ) where

import LLVM.AST

import LLVM.AST.Tagged.Name
import LLVM.AST.Tagged.Type (Type(..), FloatingPointFormat(..))
import LLVM.AST.Tagged.Global
import LLVM.AST.Tagged.Operand
import LLVM.AST.Tagged.Instruction
import LLVM.AST.Tagged.DataLayout

