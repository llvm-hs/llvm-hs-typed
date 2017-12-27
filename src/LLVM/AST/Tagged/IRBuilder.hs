{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeApplications #-}
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

  sext,
  zext,
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

import GHC.TypeLits
import GHC.Exts (Constraint)
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

fsub
  :: IR.MonadIRBuilder m
  => (Operand ::: (IntegerType' t))
  -> (Operand ::: (IntegerType' t))
  -> m (Operand ::: (IntegerType' t))
fsub a b = IR.fsub (coerce a) (coerce b) >>= pure . coerce

fdiv
  :: IR.MonadIRBuilder m
  => (Operand ::: (IntegerType' t))
  -> (Operand ::: (IntegerType' t))
  -> m (Operand ::: (IntegerType' t))
fdiv a b = IR.fdiv (coerce a) (coerce b) >>= pure . coerce

frem
  :: IR.MonadIRBuilder m
  => (Operand ::: (IntegerType' t))
  -> (Operand ::: (IntegerType' t))
  -> m (Operand ::: (IntegerType' t))
frem a b = IR.frem (coerce a) (coerce b) >>= pure . coerce



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

sub
  :: IR.MonadIRBuilder m
  => (Operand ::: (IntegerType' t))
  -> (Operand ::: (IntegerType' t))
  -> m (Operand ::: (IntegerType' t))
sub a b = IR.sub (coerce a) (coerce b) >>= pure . coerce

udiv
  :: IR.MonadIRBuilder m
  => (Operand ::: (IntegerType' t))
  -> (Operand ::: (IntegerType' t))
  -> m (Operand ::: (IntegerType' t))
udiv a b = IR.udiv (coerce a) (coerce b) >>= pure . coerce

sdiv
  :: IR.MonadIRBuilder m
  => (Operand ::: (IntegerType' t))
  -> (Operand ::: (IntegerType' t))
  -> m (Operand ::: (IntegerType' t))
sdiv a b = IR.sdiv (coerce a) (coerce b) >>= pure . coerce

urem
  :: IR.MonadIRBuilder m
  => (Operand ::: (IntegerType' t))
  -> (Operand ::: (IntegerType' t))
  -> m (Operand ::: (IntegerType' t))
urem a b = IR.urem (coerce a) (coerce b) >>= pure . coerce

shl
  :: IR.MonadIRBuilder m
  => (Operand ::: (IntegerType' t))
  -> (Operand ::: (IntegerType' t))
  -> m (Operand ::: (IntegerType' t))
shl a b = IR.shl (coerce a) (coerce b) >>= pure . coerce

lshr
  :: IR.MonadIRBuilder m
  => (Operand ::: (IntegerType' t))
  -> (Operand ::: (IntegerType' t))
  -> m (Operand ::: (IntegerType' t))
lshr a b = IR.lshr (coerce a) (coerce b) >>= pure . coerce

ashr
  :: IR.MonadIRBuilder m
  => (Operand ::: (IntegerType' t))
  -> (Operand ::: (IntegerType' t))
  -> m (Operand ::: (IntegerType' t))
ashr a b = IR.ashr (coerce a) (coerce b) >>= pure . coerce

and
  :: IR.MonadIRBuilder m
  => (Operand ::: (IntegerType' t))
  -> (Operand ::: (IntegerType' t))
  -> m (Operand ::: (IntegerType' t))
and a b = IR.and (coerce a) (coerce b) >>= pure . coerce

or
  :: IR.MonadIRBuilder m
  => (Operand ::: (IntegerType' t))
  -> (Operand ::: (IntegerType' t))
  -> m (Operand ::: (IntegerType' t))
or a b = IR.or (coerce a) (coerce b) >>= pure . coerce

xor
  :: IR.MonadIRBuilder m
  => (Operand ::: (IntegerType' t))
  -> (Operand ::: (IntegerType' t))
  -> m (Operand ::: (IntegerType' t))
xor a b = IR.or (coerce a) (coerce b) >>= pure . coerce

sext
  :: forall width1 width2. (Known width2, width1 <= width2)
  => forall m. IR.MonadIRBuilder m
  => (Operand ::: (IntegerType' width1))
  -> m (Operand ::: (IntegerType' width2))
sext a = IR.sext (coerce a) (val @_ @(IntegerType' width2)) >>= pure . coerce

zext
  :: forall width1 width2. (Known width2, width1 <= width2)
  => forall m. IR.MonadIRBuilder m
  => (Operand ::: (IntegerType' width1))
  -> m (Operand ::: (IntegerType' width2))
zext a = IR.zext (coerce a) (val @_ @(IntegerType' width2)) >>= pure . coerce

fptoui
  :: forall fpt width. Known width
  => forall m. IR.MonadIRBuilder m
  => (Operand ::: (FloatingPointType' fpt))
  -> m (Operand ::: (IntegerType' width))
fptoui a = IR.fptoui (coerce a) (val @_ @(IntegerType' width)) >>= pure . coerce

fptosi
  :: forall fpt width. Known width
  => forall m. IR.MonadIRBuilder m
  => (Operand ::: (FloatingPointType' fpt))
  -> m (Operand ::: (IntegerType' width))
fptosi a = IR.fptosi (coerce a) (val @_ @(IntegerType' width)) >>= pure . coerce
