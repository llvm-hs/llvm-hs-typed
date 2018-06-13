{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module Main where

import GHC.TypeLits
import LLVM.Prelude
import LLVM.AST.Tagged.Operand
import qualified LLVM.AST.Tagged.Constant as TC
import LLVM.AST.Tagged.Global
import LLVM.AST.Tagged.Tag
import LLVM.AST.TypeLevel.Type
import qualified LLVM.AST as AST
import qualified LLVM.AST.Type as AST
import qualified LLVM.AST.Float as F
import qualified LLVM.AST.Constant as C
import qualified LLVM.AST.IntegerPredicate as P

import LLVM.AST.Tagged.IRBuilder as TBuilder
import qualified LLVM.IRBuilder as Builder

import Data.HVect

simple :: AST.Module
simple = Builder.buildModule "exampleModule" $ mdo
  TBuilder.function @(IntegerType' 32) @'[ '(IntegerType' 32, 'ParameterName' "a")] "f" $ \(a :&: HNil)  -> mdo
    entry <- block `named` "entry"
    cond <- icmp P.EQ a (constantOperand (TC.int 0))
    condBr cond ifThen ifElse
    ifThen <- block
    trVal <- add a (constantOperand (TC.int 0))
    br ifExit
    ifElse <- block `named` "if.else"
    flVal <- add a (constantOperand (TC.int 0))
    br ifExit
    ifExit <- block `named` "if.exit"
    r <- phi [(trVal, ifThen), (flVal, ifElse)]
    ret r

main :: IO ()
main = print simple
