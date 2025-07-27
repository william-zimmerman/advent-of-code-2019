{-# LANGUAGE ScopedTypeVariables #-}

module Day5 (runDay5) where

import Data.Char (digitToInt)
import Data.List.Split (splitOn)

type Address = Int
type AddressableList a = [(Address, a)]
type AddressableMemory = AddressableList Int

data ParameterMode = Position | Immediate deriving (Show)

type InstructionParameter = Int
type ParameterAndMode = (InstructionParameter, ParameterMode)

newtype InstructionPointer = MkInstructionPointer Int deriving (Show)

data Instruction = Add | Multiply | Halt | Read | Write

type InstructionAndParameters = (Instruction, [ParameterAndMode])

numberOfParams :: Instruction -> Int
numberOfParams Add = 3
numberOfParams Multiply = 3
numberOfParams Halt = 0
numberOfParams Read = 1
numberOfParams Write = 1

runProgram :: (Int, Int) -> AddressableMemory -> Maybe AddressableMemory
runProgram initialInputs memory =
    let adjustedMemory = replace 2 (snd initialInputs) (replace 1 (fst initialInputs) memory)
     in recurseProgram start adjustedMemory

recurseProgram :: InstructionPointer -> AddressableMemory -> Maybe AddressableMemory
recurseProgram instructionPointer memory = do
    (instruction, parameterAndModes) <- getInstructionAndParams instructionPointer memory
    inputs <- resolveParameters parameterAndModes memory
    case instruction of
        Halt -> Just memory
        other -> recurseProgram (nextInstructionPointer instruction instructionPointer) (applyInstruction other inputs memory)

start :: InstructionPointer
start = MkInstructionPointer 0

createAddressableList :: [a] -> AddressableList a
createAddressableList = zip [0 ..]

replace :: Address -> a -> AddressableList a -> AddressableList a
replace _ _ [] = []
replace position newValue ((p', currentValue) : xs)
    | position == p' = (position, newValue) : xs
    | otherwise = (p', currentValue) : replace position newValue xs

getInstructionAndParams :: InstructionPointer -> AddressableMemory -> Maybe InstructionAndParameters
getInstructionAndParams instructionPointer@(MkInstructionPointer address) memory = do
    opcode <- lookup address memory
    (instruction, parameterModes) <- parseOpcode opcode
    parameters <- getInstructionParams instruction instructionPointer memory
    let parametersAndModes = zip parameters (parameterModes <> repeat Position)
    return (instruction, parametersAndModes)

parseOpcode :: Int -> Maybe (Instruction, [ParameterMode])
parseOpcode value = do
    let opcode = value `mod` 100
    let modeInts = value `div` 100
    instruction <- parseInstruction opcode
    parameterModes <- parseModes modeInts
    return (instruction, parameterModes)

parseInstruction :: Int -> Maybe Instruction
parseInstruction 1 = Just Add
parseInstruction 2 = Just Multiply
parseInstruction 99 = Just Halt
parseInstruction _ = Nothing

parseModes :: Int -> Maybe [ParameterMode]
parseModes value =
    let valueAsString = show value
     in mapM (parseMode . digitToInt) (reverse valueAsString)

parseMode :: Int -> Maybe ParameterMode
parseMode 0 = Just Position
parseMode 1 = Just Immediate
parseMode _ = Nothing

getInstructionParams :: Instruction -> InstructionPointer -> AddressableMemory -> Maybe [InstructionParameter]
getInstructionParams instruction (MkInstructionPointer address) memory =
    let
        getInstructionParams' :: Int -> Address -> AddressableMemory -> [Maybe InstructionParameter]
        getInstructionParams' 0 _ _ = []
        getInstructionParams' count currentAddress memory =
            lookup currentAddress memory : getInstructionParams' (count - 1) (currentAddress + 1) memory
     in
        sequence $ getInstructionParams' (numberOfParams instruction) (address + 1) memory

nextInstructionPointer :: Instruction -> InstructionPointer -> InstructionPointer
nextInstructionPointer instruction (MkInstructionPointer address) =
    MkInstructionPointer (address + numberOfParams instruction + 1)

applyInstruction :: Instruction -> [Int] -> AddressableMemory -> AddressableMemory
applyInstruction Add inputs memory = applyInstructionParams3 (+) inputs memory
applyInstruction Multiply inputs memory = applyInstructionParams3 (*) inputs memory
applyInstruction Halt _ memory = memory
applyInstruction Read _ memory = undefined
applyInstruction Write _ memory = undefined

resolveParameters :: [ParameterAndMode] -> AddressableMemory -> Maybe [Int]
resolveParameters paramsAndModes memory = mapM (resolveParameter memory) paramsAndModes

resolveParameter :: AddressableMemory -> ParameterAndMode -> Maybe Int
resolveParameter _ (value, Immediate) = Just value
resolveParameter memory (address, Position) = lookup address memory

applyInstructionParams3 :: (Int -> Int -> Int) -> [Int] -> AddressableList Int -> AddressableList Int
applyInstructionParams3 f inputs list =
    let input1 = head inputs
        input2 = inputs !! 1
        outputAddress = inputs !! 2
        computedValue = f input1 input2
     in replace outputAddress computedValue list

getAnswer :: AddressableList a -> Maybe a
getAnswer = lookup 0

runDay5 :: IO ()
runDay5 = do
    strings <- concatMap (splitOn ",") . lines <$> readFile "resources/day2.txt"
    let ints :: [Int] = map read strings
    let memory = createAddressableList ints
    print (runProgram (12, 2) memory)
    return ()
