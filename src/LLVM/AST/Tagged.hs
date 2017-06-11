{- |

This module provides a type-safe variant of "LLVM.AST".

The tagged variant of the type-safe API of "LLVM.AST" mirrors the untyped
API using the following principle:

For every constructor @Con@ in the untyped API in module @LLVM.AST.Foo@
there is a type-safe smart constructor @con@ in the corresponding module in
@LLVM.AST.Tagged.Foo@. All value level arguments of type 'Type' disappear
(and turn into 'Known' constraints). The other arguments, where it makes sense,
get tagged with their LLVM type, on the Haskell type level. Finally, the
types of different arguments (multiple operators) are connected to ensure
type safety. This connection is sometimes done by simply using the same type variable
to ensure that two types are the same, sometimes using more complex
type-level machinery.

Example 1:

The constructor

    FAdd :: Constant -> Constant -> Constant

turns into

    fadd = forall fpt.
      Constant ::: (FloatingPointType' fpt) ->
      Constant ::: (FloatingPointType' fpt) ->
      Constant ::: (FloatingPointType' fpt)

Which ensures that the arguments and the return value are all of floating point
type, and furthermore that all of them use the same floating point format.

Example 2:

The constructor

    FPExt :: Constant -> Type -> Constant

turns into

    fpext :: forall fpt1 fpt2. Known fpt2 =>
        (Known fpt2, BitSizeOfFP fpt1 <= BitSizeOfFP fpt2) =>
        Constant ::: FloatingPointType' fpt1 ->
        Constant ::: FloatingPointType' fpt2

which ensures that the argument and the return value are both floating point
values, and further more that the argument has a smaller bit size than the
result. This uses the type-level function @BitSizeOfFP :: FloatingPointType ->
Nat@ that returns the bit width of a givne @FloatingPointType@.
-}
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
import LLVM.AST.Tagged.Type (Type(..), FloatingPointType(..))
import LLVM.AST.Tagged.Global
import LLVM.AST.Tagged.Operand
import LLVM.AST.Tagged.Instruction
import LLVM.AST.Tagged.DataLayout

