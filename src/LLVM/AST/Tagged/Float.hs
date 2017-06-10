{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}

-- | This module provides a type-safe variant of "LLVM.AST.Float".
module LLVM.AST.Tagged.Float where

import Data.Word

import LLVM.AST.Float
import LLVM.AST.Type (FloatingPointType(..))
import LLVM.AST.Tagged.Tag

half :: Word16 -> SomeFloat :::: HalfFP
half w = assertLLVMType (Half w)

single :: Float -> SomeFloat :::: FloatFP
single f = assertLLVMType (Single f)

double :: Double -> SomeFloat :::: DoubleFP
double d = assertLLVMType (Double d)

quadruple :: Word64 -> Word64 -> SomeFloat :::: FP128FP
quadruple w1 w2 = assertLLVMType (Quadruple w1 w2)

x86_fp80 :: Word16 -> Word64 -> SomeFloat :::: X86_FP80FP
x86_fp80 w1 w2 = assertLLVMType (X86_FP80 w1 w2)

ppc_fp128 :: Word64 -> Word64 -> SomeFloat :::: PPC_FP128FP
ppc_fp128 w1 w2 = assertLLVMType (PPC_FP128 w1 w2)
