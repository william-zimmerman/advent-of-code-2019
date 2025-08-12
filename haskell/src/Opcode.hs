module Opcode (
    parseOpcodeValue,
    instructionCode,
    Opcode,
    ParameterMode (..),
    orderedParameterModes,
    defaultMode,
) where

import Data.Char (digitToInt)
import Text.Printf (printf)

type InstructionCode = Int

data Opcode = MkOpcode InstructionCode [ParameterMode] deriving (Show)

data ParameterMode = Position | Immediate deriving (Show)

defaultMode :: ParameterMode
defaultMode = Position

parseOpcodeValue :: Int -> Either String Opcode
parseOpcodeValue value = do
    let instructionCode' = value `mod` 100
    let modeCodes = value `div` 100
    parameterModes <- parseModeCodes modeCodes
    return (MkOpcode instructionCode' parameterModes)

instructionCode :: Opcode -> InstructionCode
instructionCode (MkOpcode instructionCode' _) = instructionCode'

orderedParameterModes :: Opcode -> [ParameterMode]
orderedParameterModes (MkOpcode _ parameterModes) = parameterModes

parseModeCodes :: Int -> Either String [ParameterMode]
parseModeCodes value =
    let valueAsString = show value
     in mapM (parseModeCode . digitToInt) (reverse valueAsString)

parseModeCode :: Int -> Either String ParameterMode
parseModeCode 0 = Right Position
parseModeCode 1 = Right Immediate
parseModeCode value = Left (printf "Unable to parse parameter mode code %s=i" value)
