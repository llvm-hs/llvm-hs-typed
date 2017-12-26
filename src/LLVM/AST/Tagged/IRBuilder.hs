{-# LANGUAGE TypeOperators #-}

module LLVM.AST.Tagged.IRBuilder (
  emitInstr,
  emitTerm,
) where

import LLVM.AST.Type
import LLVM.AST.TypeLevel.Type
import LLVM.AST.TypeLevel.Utils
import LLVM.AST.Tagged.Tag
import LLVM.AST.Constant
import LLVM.AST.Operand
import LLVM.AST.Instruction
import LLVM.AST.Name (Name)
import LLVM.AST.Float (SomeFloat)
import LLVM.AST.IntegerPredicate (IntegerPredicate)
import LLVM.AST.FloatingPointPredicate (FloatingPointPredicate)

import Data.Coerce
import qualified LLVM.IRBuilder as IR

emitInstr :: IR.MonadIRBuilder m => Type -> (Instruction ::: t) -> m Operand
emitInstr ty instr = IR.emitInstr ty (coerce instr)

emitTerm :: IR.MonadIRBuilder m => (Terminator ::: t) -> m ()
emitTerm instr = IR.emitTerm (coerce instr)
