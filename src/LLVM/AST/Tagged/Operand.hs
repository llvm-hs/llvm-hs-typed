{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

-- | This module provides a type-safe variant of "LLVM.AST.Operand".
-- It is currently a stub
module LLVM.AST.Tagged.Operand (
  module LLVM.AST.Operand,
  constantOperand,
) where

import Data.Coerce
import LLVM.AST.Tagged.Tag
import LLVM.AST.TypeLevel.Type
import LLVM.AST.Operand
import LLVM.AST.Constant

constantOperand
  :: forall t. Known t
  => (Constant ::: t)
  -> Operand ::: t
constantOperand c = assertLLVMType (ConstantOperand (coerce c))
