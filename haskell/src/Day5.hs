{-# LANGUAGE ScopedTypeVariables #-}

module Day5 (runDay5) where

import qualified AddressableList as AL (Address, AddressableList, create, replace)
import Data.List (uncons)
import Data.List.Split (splitOn)
import qualified Opcode as O (
    Instruction (Add, Halt, Multiply, Read, Write),
    Opcode,
    ParameterMode (Immediate, Position),
    defaultMode,
    instruction,
    orderedParameterModes,
    parseOpcodeValue,
 )

type AddressableMemory = AL.AddressableList Int

data InstructionParameter = Immediate Int | Position Int

newtype InstructionPointer = MkInstructionPointer Int deriving (Show)

type InstructionAndParameters = (O.Instruction, [InstructionParameter])

numberOfParams :: O.Instruction -> Int
numberOfParams O.Add = 3
numberOfParams O.Multiply = 3
numberOfParams O.Halt = 0
numberOfParams O.Read = 1
numberOfParams O.Write = 1

runProgram :: (Int, Int) -> AddressableMemory -> Maybe AddressableMemory
runProgram initialInputs memory =
    let adjustedMemory = AL.replace 2 (snd initialInputs) (AL.replace 1 (fst initialInputs) memory)
     in recurseProgram start adjustedMemory

recurseProgram :: InstructionPointer -> AddressableMemory -> Maybe AddressableMemory
recurseProgram instructionPointer memory = do
    (instruction, parameterAndModes) <- getInstructionAndParams instructionPointer memory
    inputs <- resolveParameters parameterAndModes memory
    case instruction of
        O.Halt -> Just memory
        actionableInstruction -> recurseProgram (nextInstructionPointer instruction instructionPointer) (applyInstruction actionableInstruction inputs memory)

start :: InstructionPointer
start = MkInstructionPointer 0

getInstructionAndParams :: InstructionPointer -> AddressableMemory -> Maybe InstructionAndParameters
getInstructionAndParams instructionPointer@(MkInstructionPointer address) memory = do
    opcodeValue <- lookup address memory
    opcode <- O.parseOpcodeValue opcodeValue
    parameters <- getInstructionParams opcode instructionPointer memory
    return (O.instruction opcode, parameters)

getInstructionParams :: O.Opcode -> InstructionPointer -> AddressableMemory -> Maybe [InstructionParameter]
getInstructionParams opcode (MkInstructionPointer address) memory =
    let
        getInstructionParams' :: Int -> [O.ParameterMode] -> AL.Address -> AddressableMemory -> [Maybe InstructionParameter]
        getInstructionParams' 0 _ _ _ = []
        getInstructionParams' count modes currentAddress memory' =
            let maybeHeadAndTail = uncons modes
                currentMode = maybe O.defaultMode fst maybeHeadAndTail
                nextModes = maybe [] snd maybeHeadAndTail
             in fmap (translate currentMode) (lookup currentAddress memory') : getInstructionParams' (count - 1) nextModes (currentAddress + 1) memory'
     in
        sequence $ getInstructionParams' (numberOfParams $ O.instruction opcode) (O.orderedParameterModes opcode) (address + 1) memory

translate :: O.ParameterMode -> Int -> InstructionParameter
translate O.Position = Position
translate O.Immediate = Immediate

nextInstructionPointer :: O.Instruction -> InstructionPointer -> InstructionPointer
nextInstructionPointer instruction (MkInstructionPointer address) =
    MkInstructionPointer (address + numberOfParams instruction + 1)

applyInstruction :: O.Instruction -> [Int] -> AddressableMemory -> AddressableMemory
applyInstruction O.Add inputs memory = applyInstructionParams3 (+) inputs memory
applyInstruction O.Multiply inputs memory = applyInstructionParams3 (*) inputs memory
applyInstruction O.Halt _ memory = memory
applyInstruction O.Read _ memory = undefined
applyInstruction O.Write _ memory = undefined

resolveParameters :: [InstructionParameter] -> AddressableMemory -> Maybe [Int]
resolveParameters params memory = mapM (resolveParameter memory) params

resolveParameter :: AddressableMemory -> InstructionParameter -> Maybe Int
resolveParameter _ (Day5.Immediate value) = Just value
resolveParameter memory (Day5.Position address) = lookup address memory

applyInstructionParams3 :: (Int -> Int -> Int) -> [Int] -> AL.AddressableList Int -> AL.AddressableList Int
applyInstructionParams3 f inputs list =
    let input1 = head inputs
        input2 = inputs !! 1
        outputAddress = inputs !! 2
        computedValue = f input1 input2
     in AL.replace outputAddress computedValue list

runDay5 :: IO ()
runDay5 = do
    strings <- concatMap (splitOn ",") . lines <$> readFile "resources/day2.txt"
    let ints :: [Int] = map read strings
    let memory = AL.create ints
    print (runProgram (12, 2) memory)
    return ()
