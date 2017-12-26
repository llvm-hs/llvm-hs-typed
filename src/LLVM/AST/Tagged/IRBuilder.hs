{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}

module LLVM.AST.Tagged.IRBuilder (
  -- ** Operands
  emitInstr,
  emitTerm,

  -- ** Instructions
  fadd,
  fmul,

  add,
  mul,
) where

import LLVM.Prelude
import LLVM.AST
import LLVM.AST.Type
import LLVM.AST.TypeLevel.Type
import LLVM.AST.TypeLevel.Utils
import LLVM.AST.Tagged.Tag
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

fadd
  :: IR.MonadIRBuilder m
  => (Operand ::: (FloatingPointType' t))
  -> (Operand ::: (FloatingPointType' t))
  -> m (Operand ::: (FloatingPointType' t))
fadd a b = IR.fadd (coerce a) (coerce b) >>= pure . coerce

fmul
  :: IR.MonadIRBuilder m
  => (Operand ::: (FloatingPointType' t))
  -> (Operand ::: (FloatingPointType' t))
  -> m (Operand ::: (FloatingPointType' t))
fmul a b = IR.fmul (coerce a) (coerce b) >>= pure . coerce

add
  :: IR.MonadIRBuilder m
  => (Operand ::: (IntegerType' t))
  -> (Operand ::: (IntegerType' t))
  -> m (Operand ::: (IntegerType' t))
add a b = IR.add (coerce a) (coerce b) >>= pure . coerce

mul
  :: IR.MonadIRBuilder m
  => (Operand ::: (IntegerType' t))
  -> (Operand ::: (IntegerType' t))
  -> m (Operand ::: (IntegerType' t))
mul a b = IR.mul (coerce a) (coerce b) >>= pure . coerce
