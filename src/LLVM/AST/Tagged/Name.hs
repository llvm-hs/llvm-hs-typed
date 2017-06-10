-- | This module provides a type-safe variant of "LLVM.AST.Name".
--
-- There is nothing type-safety-specific here, so this just re-exports "LLVM.AST.Name"
module LLVM.AST.Tagged.Name (module LLVM.AST.Name) where

import LLVM.AST.Name
