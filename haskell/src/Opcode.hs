module Opcode (
    parseOpcodeValue,
    instruction,
    Instruction (..),
    Opcode,
    ParameterMode (..),
    orderedParameterModes,
    defaultMode,
) where

import Data.Char (digitToInt)

data Opcode = MkOpcode Instruction [ParameterMode] deriving (Show)

data Instruction = Add | Multiply | Halt | Read | Write deriving (Show)
data ParameterMode = Position | Immediate deriving (Show)

defaultMode :: ParameterMode
defaultMode = Position

parseOpcodeValue :: Int -> Maybe Opcode
parseOpcodeValue value = do
    let instructionCode = value `mod` 100
    let modeCodes = value `div` 100
    instruction' <- parseInstructionCode instructionCode
    parameterModes <- parseModeCodes modeCodes
    return (MkOpcode instruction' parameterModes)

instruction :: Opcode -> Instruction
instruction (MkOpcode instruction' _) = instruction'

orderedParameterModes :: Opcode -> [ParameterMode]
orderedParameterModes (MkOpcode _ parameterModes) = parameterModes

parseInstructionCode :: Int -> Maybe Instruction
parseInstructionCode 1 = Just Add
parseInstructionCode 2 = Just Multiply
parseInstructionCode 99 = Just Halt
parseInstructionCode 3 = Just Read
parseInstructionCode 4 = Just Write
parseInstructionCode _ = Nothing

parseModeCodes :: Int -> Maybe [ParameterMode]
parseModeCodes value =
    let valueAsString = show value
     in mapM (parseModeCode . digitToInt) (reverse valueAsString)

parseModeCode :: Int -> Maybe ParameterMode
parseModeCode 0 = Just Position
parseModeCode 1 = Just Immediate
parseModeCode _ = Nothing
