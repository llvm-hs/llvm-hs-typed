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
{-# LANGUAGE OverloadedStrings #-}

module Standalone where

-- AST
import GHC.TypeLits
import LLVM.Prelude
import LLVM.AST.Tagged
import LLVM.AST.Constant
import LLVM.AST.Tagged.Global
import LLVM.AST.Tagged.Constant
import LLVM.AST.Tagged.Tag
import LLVM.AST.TypeLevel.Type
import qualified LLVM.AST as AST
import qualified LLVM.AST.Global as AST

c0 :: Constant ::: IntegerType' 32
c0 = int 42

named :: forall (t :: Type'). ShortByteString -> Name ::: t
named s = assertLLVMType $ AST.Name s

type ArgTys = [(IntegerType' 32), (IntegerType' 32)]
type RetTy = IntegerType' 32

defAdd :: Global
defAdd = function nm (params, False) [body]
  where
    nm :: Name ::: (PointerType' (FunctionType' (IntegerType' 32) ArgTys) ('AddrSpace' 0))
    nm = named "add"

    p1 :: Parameter ::: (IntegerType' 32)
    p1 = parameter (named "a") []

    p2 :: Parameter ::: (IntegerType' 32)
    p2 = parameter (named "b") []

    body :: BasicBlock ::: IntegerType' 32
    body = basicBlock "entry" [] (doRet (ret (constantOperand c0) []))

    params :: Parameter :::* ArgTys
    params = p1 :* p2 :* tnil
