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
-- It is currently a stub
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
ret :: Operand ::: t -> InstructionMetadata -> Terminator ::: t
ret o = coerce Ret (Just o)

retVoid :: InstructionMetadata -> Terminator ::: VoidType'
retVoid = coerce (Ret Nothing)

condBr ::
    Operand ::: IntegerType' 1 ->
    Name ::: LabelType' ->
    Name ::: LabelType' ->
    InstructionMetadata ->
    Terminator ::: t
condBr = coerce CondBr

br ::
    Name ::: LabelType' ->
    InstructionMetadata ->
    Terminator ::: t
br = coerce Br

switch ::
    Operand ::: t ->
    Name ::: LabelType' ->
    [(Constant ::: t, Name ::: LabelType')] ->
    InstructionMetadata ->
    Terminator ::: t2
switch = coerce Switch

indirectBr ::
    Operand ::: PointerType' (IntegerType' 8) as ->
    [( Name ::: LabelType')] ->
    InstructionMetadata ->
    Terminator ::: t2
indirectBr = coerce IndirectBr

invoke ::
    CallingConvention ->
    [PA.ParameterAttribute] ->
    CallableOperand ::: PointerType' (FunctionType' ret_ty args_tys) as ->
    (Operand, [PA.ParameterAttribute]) :::* args_tys ->
    [Either FA.GroupID FA.FunctionAttribute] ->
    Name ::: LabelType' ->
    Name ::: LabelType' ->
    InstructionMetadata ->
    Terminator ::: t2
invoke = coerce Invoke

-- | It is not checked that the type of the operand matches the type of
-- landingpads in this function.
resume ::
    Operand ::: t ->
    InstructionMetadata ->
    Terminator ::: t2
resume = coerce Resume

unreachable ::
    InstructionMetadata ->
    Terminator ::: t
unreachable = coerce Unreachable

cleanupRet ::
    Operand ::: TokenType' ->
    Maybe (Name ::: LabelType') ->
    InstructionMetadata ->
    Terminator ::: t2
cleanupRet = coerce CleanupRet

catchRet ::
    Operand ::: TokenType' ->
    Name ::: LabelType' ->
    InstructionMetadata ->
    Terminator ::: t2
catchRet = coerce CatchRet

catchSwitch ::
    Operand ::: TokenType' ->
    NonEmpty (Name ::: LabelType') ->
    Maybe (Name ::: LabelType') ->
    InstructionMetadata ->
    Terminator ::: t2
catchSwitch = coerce CatchSwitch



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
do' :: Instruction ::: VoidType' -> Named Instruction
do' = coerce (Do :: Instruction -> Named Instruction)

-- | Do for terminators
doRet :: forall (t :: Type'). Terminator ::: t -> Named (Terminator ::: t)
doRet = coerce (Do :: Terminator -> Named Terminator)

