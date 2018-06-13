{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Example2 where

import GHC.TypeLits
import LLVM.Prelude
import LLVM.AST.Constant
import LLVM.AST.Tagged.Global
import LLVM.AST.Tagged.Tag
import LLVM.AST.TypeLevel.Type
import qualified LLVM.AST as AST
import qualified LLVM.AST.Type as AST
import qualified LLVM.AST.Global as AST
import qualified LLVM.AST.Tagged as AST

import LLVM.AST.Tagged.IRBuilder as TBuilder
import qualified LLVM.IRBuilder as Builder

import Data.HVect

simple :: AST.Module
simple = Builder.buildModule "exampleModule" $ do
    func
  where
  func :: Builder.ModuleBuilder (AST.Operand ::: IntegerType' 32)
  func =
    TBuilder.function @(IntegerType' 32) @'[ '(IntegerType' 32, 'ParameterName' "a"), '(IntegerType' 32, 'ParameterName' "b")] "add" $ \(a :&: b :&: HNil) -> do
      entry <- block `named` "entry"; do
        c <- add a b
        ret c
