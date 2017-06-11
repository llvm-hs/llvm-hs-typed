{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE UndecidableInstances #-}

-- | This module provides a type-safe variant of "LLVM.AST.Instruction".
--
-- Note that the smart constructors create @Named Terminators@ directly. Almost
-- all of them do not accept a name anyways, with the exception of 'invoke',
-- which now takes the @Name@ as an argument.
module LLVM.AST.Tagged.Instruction where

import Data.Coerce

import LLVM.AST.Tagged.Tag
import LLVM.AST.Operand
import LLVM.AST.Constant
import LLVM.AST.TypeLevel.Type
import LLVM.AST.Instruction
import LLVM.AST.Tagged.Name

import Data.List.NonEmpty

import LLVM.AST.CallingConvention (CallingConvention)
import qualified LLVM.AST.ParameterAttribute as PA (ParameterAttribute)
import qualified LLVM.AST.FunctionAttribute as FA (FunctionAttribute, GroupID)


-- TODO: Lots of stuff missing

-- | We distuingish between returning from a @void@ function and otherwise by
-- two different smart constructors (instead of passing a @Maybe@ to @Ret@).
ret :: Operand ::: t -> InstructionMetadata -> Named Terminator ::: t
ret o im = doRet $ coerce Ret (Just o) im

retVoid :: InstructionMetadata -> Named Terminator ::: VoidType'
retVoid im = doRet $ coerce (Ret Nothing) im

condBr ::
    Operand ::: IntegerType' 1 ->
    Name ::: LabelType' ->
    Name ::: LabelType' ->
    InstructionMetadata ->
    Named Terminator ::: t
condBr o1 n1 n2 im = doRet $ coerce CondBr o1 n1 n2 im

br ::
    Name ::: LabelType' ->
    InstructionMetadata ->
    Named Terminator ::: t
br n im = doRet $ coerce Br n im

switch ::
    Operand ::: t ->
    Name ::: LabelType' ->
    [(Constant ::: t, Name ::: LabelType')] ->
    InstructionMetadata ->
    Named Terminator ::: t2
switch o n targets im = doRet $ coerce Switch o n targets im

indirectBr ::
    Operand ::: PointerType' (IntegerType' 8) as ->
    [( Name ::: LabelType')] ->
    InstructionMetadata ->
    Named Terminator ::: t2
indirectBr o ns im = doRet $ coerce IndirectBr o ns im

invoke ::
    Name ::: ret_ty ->
    CallingConvention ->
    [PA.ParameterAttribute] ->
    CallableOperand ::: PointerType' (FunctionType' ret_ty args_tys) as ->
    (Operand, [PA.ParameterAttribute]) :::* args_tys ->
    [Either FA.GroupID FA.FunctionAttribute] ->
    Name ::: LabelType' ->
    Name ::: LabelType' ->
    InstructionMetadata ->
    Named Terminator ::: t2
invoke n cc pas o os fas n1 n2 im
    = assertLLVMType $ coerce n := coerce Invoke cc pas o os fas n1 n2 im

-- | It is not checked that the type of the operand matches the type of
-- landingpads in this function.
resume ::
    Operand ::: t ->
    InstructionMetadata ->
    Named Terminator ::: t2
resume o im = doRet $ coerce Resume o im

unreachable ::
    InstructionMetadata ->
    Named Terminator ::: t
unreachable im = doRet $ coerce Unreachable im

cleanupRet ::
    Operand ::: TokenType' ->
    Maybe (Name ::: LabelType') ->
    InstructionMetadata ->
    Named Terminator ::: t2
cleanupRet o mbn im = doRet $ coerce CleanupRet o mbn im

catchRet ::
    Operand ::: TokenType' ->
    Name ::: LabelType' ->
    InstructionMetadata ->
    Named Terminator ::: t2
catchRet o n im = doRet $ coerce CatchRet o n im

catchSwitch ::
    Operand ::: TokenType' ->
    NonEmpty (Name ::: LabelType') ->
    Maybe (Name ::: LabelType') ->
    InstructionMetadata ->
    Named Terminator ::: t2
catchSwitch o ns n im = doRet $ coerce CatchSwitch o ns n im



-- | This is the type-safe type corresponding to @Named Instruction@. It
-- enforces that an instruction has a name if and only if it is not a void
-- instruction, and that the name and instruction have the same type.
--
-- The returned 'Named Instruction' does not carry a type, because it is not
-- useful in any way.
name :: forall (t :: Type').  NonVoid t =>
    Name ::: t ->
    Instruction ::: t ->
    Named Instruction
name = coerce ((:=) :: Name -> Instruction -> Named Instruction)

-- | If you do have a void instruction, you must use 'do'' and not pass a name
-- to it.
--
do' :: Instruction ::: VoidType' -> Named Instruction
do' = coerce (Do :: Instruction -> Named Instruction)

-- Local helper, for Terminators
doRet = assertLLVMType . Do

