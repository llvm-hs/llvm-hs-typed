{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module LLVM.AST.Tagged.IRBuilder (
  -- ** Operands
  emitInstr,
  emitInstrVoid,
  emitTerm,
  emitBlockStart,
  block,
  fresh,
  freshName,
  freshUnName,
  named,

  -- ** Instructions
  fadd,
  fmul,
  fsub,
  fdiv,
  frem,

  add,
  mul,
  sub,
  udiv,
  sdiv,
  urem,
  shl,
  lshr,
  ashr,
  and,
  or,
  xor,
  sext,
  zext,
  fptoui,
  fptosi,
  sitofp,
  uitofp,
  gep,

  trunc,
  inttoptr,
  ptrtoint,

  icmp,
  fcmp,

  bitcast,
  bitcastPtr,

  extractElement,
  insertElement,
  shuffleVector,
  extractValue,
  insertValue,

  br,
  ret,
  condBr,
  switch,
  phi,
  select,
  IR.unreachable,
  IR.retVoid,
) where

import LLVM.Prelude hiding (and, or)
import LLVM.AST
import LLVM.AST.Type
import LLVM.AST.Constant
import LLVM.AST.TypeLevel.Type
import LLVM.AST.TypeLevel.Utils
import LLVM.AST.Tagged.Tag
import LLVM.AST.Tagged.Constant (GEP_Args, GEP_Res, NotNull, ValueAt, getElementPtr, getGEPArgs)
import LLVM.AST.Operand
import LLVM.AST.Instruction
import LLVM.AST.Name (Name)
import LLVM.AST.Float (SomeFloat)
import LLVM.AST.IntegerPredicate (IntegerPredicate)
import LLVM.AST.FloatingPointPredicate (FloatingPointPredicate)
import qualified LLVM.AST.IntegerPredicate as IP
import qualified LLVM.AST.FloatingPointPredicate as FP

import GHC.TypeLits
import GHC.Exts (Constraint)
import Data.Coerce
import qualified LLVM.IRBuilder as IR

-------------------------------------------------------------------------------
-- Builder
-------------------------------------------------------------------------------

emitInstr :: IR.MonadIRBuilder m => Type -> (Instruction ::: t) -> m Operand
emitInstr ty instr = IR.emitInstr ty (coerce instr)

emitInstrVoid :: IR.MonadIRBuilder m => (Instruction ::: t) -> m ()
emitInstrVoid instr = IR.emitInstrVoid (coerce instr)

emitTerm :: IR.MonadIRBuilder m => (Terminator ::: t) -> m ()
emitTerm instr = IR.emitTerm (coerce instr)

emitBlockStart :: IR.MonadIRBuilder m => (Name ::: LabelType') -> m ()
emitBlockStart instr = IR.emitBlockStart (coerce instr)

block :: IR.MonadIRBuilder m => m (Name ::: LabelType')
block = IR.block >>= pure . coerce

fresh :: IR.MonadIRBuilder m => m (Name ::: t)
fresh = IR.fresh >>= pure . coerce

freshName :: IR.MonadIRBuilder m => ShortByteString -> m (Name ::: t)
freshName prefix = IR.freshName prefix >>= pure . coerce

freshUnName :: IR.MonadIRBuilder m => m (Name ::: t)
freshUnName = IR.freshUnName >>= pure . coerce

named :: IR.MonadIRBuilder m => m (r ::: t) -> ShortByteString -> m (r ::: t)
named m = IR.named m

-------------------------------------------------------------------------------
-- Instructions
-------------------------------------------------------------------------------

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

uitofp
  :: forall fpt width. Known width
  => forall m. IR.MonadIRBuilder m
  => (Operand ::: (FloatingPointType' fpt))
  -> m (Operand ::: (IntegerType' width))
uitofp a = IR.uitofp (coerce a) (val @_ @(IntegerType' width)) >>= pure . coerce

sitofp
  :: forall fpt width. Known width
  => forall m. IR.MonadIRBuilder m
  => (Operand ::: (FloatingPointType' fpt))
  -> m (Operand ::: (IntegerType' width))
sitofp a = IR.sitofp (coerce a) (val @_ @(IntegerType' width)) >>= pure . coerce

trunc
  :: forall width1 width2. (Known width2, width1 <= width2)
  => forall m. IR.MonadIRBuilder m
  => (Operand ::: (IntegerType' width1))
  -> m (Operand ::: (IntegerType' width2))
trunc a = IR.trunc (coerce a) (val @_ @(IntegerType' width2)) >>= pure . coerce

ptrtoint
  :: forall width t as. (Known width)
  => forall m. IR.MonadIRBuilder m
  => (Operand ::: PointerType' t as)
  -> m (Operand ::: IntegerType' width)
ptrtoint a = IR.ptrtoint (coerce a) (val @_ @(IntegerType' width)) >>= pure . coerce

inttoptr
  :: forall width t as. (Known width)
  => forall m. IR.MonadIRBuilder m
  => (Operand ::: IntegerType' width)
  -> m (Operand ::: PointerType' t as)
inttoptr a = IR.inttoptr (coerce a) (val @_ @(IntegerType' width)) >>= pure . coerce

fptrunc :: forall fpt1 fpt2 m.
    (Known fpt2, BitSizeOfFP fpt2 <= BitSizeOfFP fpt1, IR.MonadIRBuilder m)
    => Operand ::: FloatingPointType' fpt1
    -> m (Operand ::: FloatingPointType' fpt2)
fptrunc a = IR.fptrunc (coerce a) (val @_ @(FloatingPointType' fpt2)) >>= pure . coerce

fpext :: forall fpt1 fpt2 m. Known fpt2 =>
  (Known fpt2, BitSizeOfFP fpt1 <= BitSizeOfFP fpt2, IR.MonadIRBuilder m)
  => Operand ::: FloatingPointType' fpt1
  -> m (Operand ::: FloatingPointType' fpt2)
fpext a = IR.fpext (coerce a) (val @_ @(FloatingPointType' fpt2)) >>= pure . coerce

icmp
  :: IR.MonadIRBuilder m
  => IP.IntegerPredicate
  -> (Operand ::: IntegerType' t)
  -> (Operand ::: IntegerType' t)
  -> m (Operand ::: IntegerType' 1)
icmp pred a b = IR.icmp pred (coerce a) (coerce b) >>= pure . coerce

fcmp
  :: IR.MonadIRBuilder m
  => FP.FloatingPointPredicate
  -> (Operand ::: FloatingPointType' t)
  -> (Operand ::: FloatingPointType' t)
  -> m (Operand ::: IntegerType' 1)
fcmp pred a b = IR.fcmp pred (coerce a) (coerce b) >>= pure . coerce

select
  :: forall t m. IR.MonadIRBuilder m
  => Operand ::: IntegerType' 1
  -> Operand ::: t
  -> Operand ::: t
  -> m (Operand ::: t)
select cond t f = IR.select (coerce cond) (coerce t) (coerce f) >>= pure . coerce

bitcast
  :: forall t1 t2 m. IR.MonadIRBuilder m
  => (Known t1, Known t2, NonAggregate t1, NonAggregate t2)
  => (Operand ::: t1)
  -> m (Operand ::: t2)
bitcast a = IR.bitcast (coerce a) (val @_ @t2) >>= pure . coerce

bitcastPtr
  :: forall t1 t2 as m. IR.MonadIRBuilder m
  => (Known as, Known t2)
  => (Operand ::: PointerType' t1 as)
  -> m (Operand ::: PointerType' t2 as)
bitcastPtr a = IR.bitcast (coerce a) (val @_ @(PointerType' t2 as)) >>= pure . coerce

br :: IR.MonadIRBuilder m => (Name ::: LabelType') -> m ()
br val = IR.br (coerce val)

ret :: IR.MonadIRBuilder m => (Operand ::: t) -> m ()
ret val = IR.ret (coerce val)

condBr
  :: IR.MonadIRBuilder m
  => (Operand ::: t)
  -> Name ::: LabelType'
  -> Name ::: LabelType'
  -> m ()
condBr cond tdest fdest = IR.condBr (coerce cond) (coerce tdest) (coerce fdest)

switch
  :: IR.MonadIRBuilder m
  => (Operand ::: t)
  -> (Name ::: t2)
  -> [(Constant ::: t, Name ::: LabelType')]
  -> m ()
switch val def dests = IR.switch (coerce val) (coerce def) (coerce dests)

phi
  :: IR.MonadIRBuilder m
  => [(Operand ::: t, Name ::: LabelType')]
  -> m (Operand ::: t)
phi dests = IR.phi (coerce dests) >>= pure . coerce

gep
  :: forall t as static_args m. IR.MonadIRBuilder m
  => (Operand ::: PointerType' t as)
  -> GEP_Args static_args
  -> m (Operand ::: GEP_Res (PointerType' t as) static_args)
gep address indices = IR.gep (coerce address) args >>= pure . coerce
  where
    args = fmap ConstantOperand (getGEPArgs indices)

insertElement
  :: forall n t width m.  IR.MonadIRBuilder m
  => Operand ::: VectorType' n t
  -> Operand ::: t
  -> Operand ::: IntegerType' width
  -> m (Operand ::: VectorType' n t)
insertElement a b c = IR.insertElement (coerce a) (coerce b) (coerce c) >>= pure . coerce

extractElement
  :: forall n t width m.  IR.MonadIRBuilder m
  => Operand ::: VectorType' n t
  -> Operand ::: IntegerType' width
  -> m (Operand ::: t)
extractElement v i = IR.extractElement (coerce v) (coerce i) >>= pure . coerce

shuffleVector
  :: forall n l t width m.  IR.MonadIRBuilder m
  => Operand ::: VectorType' n t
  -> Operand ::: VectorType' n t
  -> Constant ::: VectorType' l (IntegerType' 32)
  -> m (Operand ::: VectorType' l t)
shuffleVector a b c = IR.shuffleVector (coerce a) (coerce b) (coerce c) >>= pure . coerce

extractValue
  :: forall t (idxs :: [Nat]) m.
  (Known idxs, NotNull idxs, IR.MonadIRBuilder m)
  => Operand ::: t
  -> m (Operand ::: ValueAt t idxs)
extractValue c = IR.extractValue (coerce c) (map fromIntegral (val @_ @idxs) :: [Word32]) >>= pure . coerce

insertValue :: forall t (idxs :: [Nat])m .
  (Known idxs, NotNull idxs, IR.MonadIRBuilder  m)
  => Operand ::: t
  -> Operand ::: ValueAt t idxs
  -> m (Operand ::: t)
insertValue c v = IR.insertValue (coerce c) (coerce v) (map fromIntegral (val @_ @idxs) :: [Word32]) >>= pure . coerce
