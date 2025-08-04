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
import Text.Printf (printf)

data Opcode = MkOpcode Instruction [ParameterMode] deriving (Show)

data Instruction = Add | Multiply | Halt | Read | Write | JumpIfTrue | JumpIfFalse | LessThan | Equals deriving (Show)
data ParameterMode = Position | Immediate deriving (Show)

defaultMode :: ParameterMode
defaultMode = Position

parseOpcodeValue :: Int -> Either String Opcode
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

parseInstructionCode :: Int -> Either String Instruction
parseInstructionCode 1 = Right Add
parseInstructionCode 2 = Right Multiply
parseInstructionCode 99 = Right Halt
parseInstructionCode 3 = Right Read
parseInstructionCode 4 = Right Write
parseInstructionCode 5 = Right JumpIfTrue
parseInstructionCode 6 = Right JumpIfFalse
parseInstructionCode 7 = Right LessThan
parseInstructionCode 8 = Right Equals
parseInstructionCode value = Left (printf "Unable to parse instruction code %i" value)

parseModeCodes :: Int -> Either String [ParameterMode]
parseModeCodes value =
    let valueAsString = show value
     in mapM (parseModeCode . digitToInt) (reverse valueAsString)

parseModeCode :: Int -> Either String ParameterMode
parseModeCode 0 = Right Position
parseModeCode 1 = Right Immediate
parseModeCode value = Left (printf "Unable to parse parameter mode code %s=i" value)
